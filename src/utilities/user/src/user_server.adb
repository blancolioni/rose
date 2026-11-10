with Rose.Server;

with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.User.Server;
with Rose.Interfaces.Writable_Directory.Client;

package body User_Server is

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

   Context   : Rose.Server.Server_Context;
   The_User  : aliased User_Object;
   User_Name : String (1 .. Max_Name_Length);
   Name_Last : Natural := 0;
   Home_Cap  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Bank_Cap  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (User_Name : String;
      Home      : Rose.Capabilities.Capability;
      Bank      : Rose.Capabilities.Capability)
   is
   begin
      Name_Last := User_Name'Length;
      User_Server.User_Name (1 .. Name_Last) := User_Name;
      Home_Cap := Home;
      Bank_Cap := Bank;
      Rose.Interfaces.User.Server.Publish (Context, The_User'Access);
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

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   --------------
   -- User_Cap --
   --------------

   overriding function User_Cap
     (Item : in out User_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.User.Server.Cap_Of (Item));

end User_Server;
