with Rose.Objects;
with Rose.Server;
with Rose.Words;

with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.User.Server;
with Rose.Interfaces.Writable_Directory.Client;

package body User_Server is

   use type Rose.Words.Word_32;

   Format : constant Rose.Words.Word_32 := 1;
   --  The version of this server's part of the state (#831):
   --
   --    the User object's identifier
   --    the length of the user's name, then its characters
   --
   --  The caps it holds, Home, Bank and Cap_Set_Service, are its
   --  command's arguments, which Rose.Replacement hands over (#901), so
   --  it names none of its own.

   --  One object serves User, and Named as its ancestor. Its state is the
   --  package's, so that a checkpoint holds it with the process's memory.

   type User_Object is
     new Rose.Interfaces.User.Server.User_Interface with null record;

   overriding function User_Cap
     (Item : in out User_Object)
      return Rose.Capabilities.Capability;

   overriding function Named_Cap
     (Item : in out User_Object)
      return Rose.Capabilities.Capability;

   overriding function Name (Item : in out User_Object) return String;

   subtype Writable_Directory_Class is
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class;

   overriding function Home_Directory
     (Item : in out User_Object)
      return Writable_Directory_Class;

   subtype Bank_Class is Rose.Interfaces.Bank.Bank_Object'Class;

   overriding function New_Session
     (Item : in out User_Object)
      return Bank_Class;

   Context    : aliased Rose.Server.Server_Context;
   The_User   : aliased User_Object;
   User_Name  : String (1 .. Max_Name_Length);
   Name_Last  : Natural := 0;
   Home_Cap   : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Bank_Cap   : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   User_Id    : Rose.Objects.Capability_Identifier := 0;
   --  The identifier that the User object had in the version that this
   --  one took over from, or 0.
   Was_Read   : Boolean := False;
   --  The state was read, from an old version or a saved one.

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (User_Name       : String;
      Home            : Rose.Capabilities.Capability;
      Bank            : Rose.Capabilities.Capability;
      Cap_Set_Service : Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability)
   is
   begin
      if not Was_Read then
         Name_Last := User_Name'Length;
         User_Server.User_Name (1 .. Name_Last) := User_Name;
      end if;

      Home_Cap := Home;
      Bank_Cap := Bank;
      Rose.Interfaces.User.Server.Attach
        (Context, The_User'Access, User_Id);
      Rose.Replacement.Attach
        (Context'Access, Write_State'Access, Cap_Set_Service);
      Control := Rose.Replacement.Replaceable_Cap;
   end Create_Server;

   --------------------
   -- Home_Directory --
   --------------------

   overriding function Home_Directory
     (Item : in out User_Object)
      return Writable_Directory_Class
   is
      pragma Unreferenced (Item);
   begin
      return Rose.Interfaces.Writable_Directory.Client.From_Cap (Home_Cap);
   end Home_Directory;

   -------------------
   -- Interface_Cap --
   -------------------

   function Interface_Cap return Rose.Capabilities.Capability is
   begin
      return Rose.Interfaces.User.Server.Cap_Of (The_User);
   end Interface_Cap;

   ----------
   -- Name --
   ----------

   overriding function Name (Item : in out User_Object) return String is
      pragma Unreferenced (Item);
   begin
      return User_Name (1 .. Name_Last);
   end Name;

   ---------------
   -- Named_Cap --
   ---------------

   overriding function Named_Cap
     (Item : in out User_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.User.Server.Cap_Of
         (Item, Rose.Interfaces.User.Named_Interface));

   -----------------
   -- New_Session --
   -----------------

   overriding function New_Session
     (Item : in out User_Object)
      return Bank_Class
   is
      pragma Unreferenced (Item);
      User_Bank : Rose.Interfaces.Bank.Client.Bank_Client :=
                    Rose.Interfaces.Bank.Client.From_Cap (Bank_Cap);
   begin
      return User_Bank.Create_Sub_Bank (0, 0);
   end New_Session;

   ----------------
   -- Read_State --
   ----------------

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps)
   is
      pragma Unreferenced (Named);
      Version : Rose.Words.Word_32;
      Length  : Natural;
   begin
      Rose.Words.Word_32'Read (Stream, Version);
      if Version /= Format then
         raise Rose.Replacement.Bad_State with "user: unknown format";
      end if;

      Rose.Objects.Capability_Identifier'Read (Stream, User_Id);
      Natural'Read (Stream, Length);
      if Length = 0 or else Length > Max_Name_Length then
         raise Rose.Replacement.Bad_State with "user: bad name";
      end if;

      for I in 1 .. Length loop
         Character'Read (Stream, User_Name (I));
      end loop;
      Name_Last := Length;
      Was_Read := True;
   end Read_State;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   ----------------
   -- Taken_Over --
   ----------------

   function Taken_Over return Boolean is (Was_Read);

   --------------
   -- User_Cap --
   --------------

   overriding function User_Cap
     (Item : in out User_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.User.Server.Cap_Of (Item));

   -----------------
   -- Write_State --
   -----------------

   procedure Write_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps)
   is
      pragma Unreferenced (Named);
   begin
      Rose.Words.Word_32'Write (Stream, Format);
      Rose.Objects.Capability_Identifier'Write
        (Stream, Rose.Interfaces.User.Server.Identifier_Of (The_User'Access));
      Natural'Write (Stream, Name_Last);
      for I in 1 .. Name_Last loop
         Character'Write (Stream, User_Name (I));
      end loop;
   end Write_State;

end User_Server;
