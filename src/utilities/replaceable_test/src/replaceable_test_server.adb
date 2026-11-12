with Ada.Strings.Unbounded;

with Rose.Interfaces.Named.Server;
with Rose.Objects;
with Rose.Server;
with Rose.Words;

package body Replaceable_Test_Server is

   use type Rose.Words.Word_32;

   Format : constant Rose.Words.Word_32 := 1;
   --  The version of this server's part of the state.

   Service_Name : constant String := "Cap_Set_Service";
   --  The name of its argument, so that a restore finds it.

   type Named_Server is
     new Rose.Interfaces.Named.Server.Named_Interface with null record;

   overriding function Named_Cap
     (Item : in out Named_Server)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Named.Server.Cap_Of (Item));

   overriding function Name (Item : in out Named_Server) return String;

   Context     : aliased Rose.Server.Server_Context;
   The_Named   : aliased Named_Server;
   Served_Name : Ada.Strings.Unbounded.Unbounded_String;
   Calls       : Natural := 0;
   Service_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Identifier  : Rose.Objects.Capability_Identifier := 0;
   --  The identifier of the Named object in the old version, or 0.
   Taken_Over  : Boolean := False;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Name            : String;
      Cap_Set_Service : Rose.Capabilities.Capability;
      Served          : out Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability;
      Saver           : out Rose.Capabilities.Capability)
   is
   begin
      if not Taken_Over then
         Served_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Service_Cap := Cap_Set_Service;
      end if;

      Rose.Interfaces.Named.Server.Attach
        (Context, The_Named'Access, Identifier);
      Rose.Replacement.Attach
        (Context'Access, Write_State'Access, Service_Cap);

      Served := Rose.Interfaces.Named.Server.Cap_Of (The_Named);
      Control := Rose.Replacement.Replaceable_Cap;
      Saver := Rose.Replacement.Saveable_Cap;
   end Create_Server;

   ----------
   -- Name --
   ----------

   overriding function Name (Item : in out Named_Server) return String is
      pragma Unreferenced (Item);
      Result : constant String :=
        Ada.Strings.Unbounded.To_String (Served_Name)
        & Natural'Image (Calls);
   begin
      Calls := Calls + 1;
      return Result;
   end Name;

   ----------------
   -- Read_State --
   ----------------

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps)
   is
      use type Rose.Capabilities.Capability;
      Version : Rose.Words.Word_32;
   begin
      Rose.Words.Word_32'Read (Stream, Version);
      if Version /= Format then
         raise Rose.Replacement.Bad_State with
           "Replaceable_Test: unknown format";
      end if;

      Rose.Objects.Capability_Identifier'Read (Stream, Identifier);
      Served_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (String'Input (Stream));
      Natural'Read (Stream, Calls);

      Service_Cap := Named.Cap (Service_Name);
      if Service_Cap = Rose.Capabilities.Null_Capability then
         raise Rose.Replacement.Bad_State with
           "Replaceable_Test: no " & Service_Name;
      end if;

      Taken_Over := True;
   end Read_State;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   -----------------
   -- Write_State --
   -----------------

   procedure Write_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps)
   is
   begin
      Rose.Words.Word_32'Write (Stream, Format);
      Rose.Objects.Capability_Identifier'Write
        (Stream,
         Rose.Interfaces.Named.Server.Identifier_Of (The_Named'Access));
      String'Output (Stream, Ada.Strings.Unbounded.To_String (Served_Name));
      Natural'Write (Stream, Calls);
      Named.Add (Service_Name, Service_Cap);
   end Write_State;

end Replaceable_Test_Server;
