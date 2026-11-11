with Ada.Strings.Unbounded;

with Rose.Interfaces.Held_Call.Client;
with Rose.Interfaces.Held_Call.Server;
with Rose.Interfaces.Named.Server;
with Rose.Invocation;
with Rose.Objects;
with Rose.Server;
with Rose.Words;

package body Replaceable_Test_Server is

   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word_32;

   Format : constant Rose.Words.Word_32 := 2;
   --  The version of this server's part of the state. Version 2 has the
   --  identifier of the Held_Call object (#879).

   Service_Name : constant String := "cap-set-service";

   Refused_Name : constant String := "refuse";
   --  A new version refuses to take over from a server with this name.

   Repeat_Name : constant String := "repeat";
   --  A helper with this name calls Wait_With, and calls it again each
   --  time it fails, until it is answered.

   type Named_Server is
     new Rose.Interfaces.Named.Server.Named_Interface with null record;

   overriding function Named_Cap
     (Item : in out Named_Server)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Named.Server.Cap_Of (Item));

   overriding function Name (Item : in out Named_Server) return String;

   type Held_Server is
     new Rose.Interfaces.Held_Call.Server.Held_Call_Interface
     with null record;

   overriding function Held_Call_Cap
     (Item : in out Held_Server)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Held_Call.Server.Cap_Of (Item));

   overriding function Wait (Item : in out Held_Server) return Natural;

   overriding function Wait_With
     (Item : in out Held_Server;
      Text : String)
      return Natural;

   overriding function Waiting (Item : in out Held_Server) return Natural;

   overriding procedure Answer
     (Item  : in out Held_Server;
      Value : Natural);

   Context         : aliased Rose.Server.Server_Context;
   The_Named       : aliased Named_Server;
   The_Held        : aliased Held_Server;
   Served_Name     : Ada.Strings.Unbounded.Unbounded_String;
   Calls           : Natural := 0;
   Service_Cap     : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Named_Id        : Rose.Objects.Capability_Identifier := 0;
   Held_Id         : Rose.Objects.Capability_Identifier := 0;
   --  The identifiers of the objects in the old version, or 0.
   Taken_Over      : Boolean := False;

   Answering       : Boolean := False;
   Answer_Value    : Natural := 0;
   --  Answer is unblocking the deferred Wait, with Answer_Value.

   Target_Cap      : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Waited          : Ada.Strings.Unbounded.Unbounded_String;
   --  A helper's target, and what its Wait returned.

   ------------
   -- Answer --
   ------------

   overriding procedure Answer
     (Item  : in out Held_Server;
      Value : Natural)
   is
      pragma Unreferenced (Item);
   begin
      Answering := True;
      Answer_Value := Value;
      Rose.Server.Unblock_Endpoint
        (Context, Rose.Interfaces.Held_Call.Wait_Endpoint);
      Rose.Server.Unblock_Endpoint
        (Context, Rose.Interfaces.Held_Call.Wait_With_Endpoint);
      Answering := False;
   end Answer;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Name            : String;
      Cap_Set_Service : Rose.Capabilities.Capability;
      Target          : Rose.Capabilities.Capability;
      Served          : out Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability;
      Held            : out Rose.Capabilities.Capability)
   is
   begin
      if not Taken_Over then
         Served_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Service_Cap := Cap_Set_Service;
      end if;
      Target_Cap := Target;

      Rose.Interfaces.Named.Server.Attach
        (Context, The_Named'Access, Named_Id);
      Rose.Interfaces.Held_Call.Server.Attach
        (Context, The_Held'Access, Held_Id);
      Rose.Replacement.Attach
        (Context'Access, Write_State'Access, Service_Cap);

      Served := Rose.Interfaces.Named.Server.Cap_Of (The_Named);
      Control := Rose.Replacement.Replaceable_Cap;
      Held := Rose.Interfaces.Held_Call.Server.Cap_Of (The_Held);
   end Create_Server;

   ----------
   -- Name --
   ----------

   overriding function Name (Item : in out Named_Server) return String is
      pragma Unreferenced (Item);
   begin
      if Target_Cap /= Rose.Capabilities.Null_Capability then
         return Ada.Strings.Unbounded.To_String (Waited);
      end if;

      declare
         Result : constant String :=
           Ada.Strings.Unbounded.To_String (Served_Name)
           & Natural'Image (Calls);
      begin
         Calls := Calls + 1;
         return Result;
      end;
   end Name;

   ----------------
   -- Read_State --
   ----------------

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      Version : Rose.Words.Word_32;
   begin
      Rose.Words.Word_32'Read (Stream, Version);
      if Version /= Format then
         raise Rose.Replacement.Bad_State with
           "Replaceable_Test: unknown format";
      end if;

      Rose.Objects.Capability_Identifier'Read (Stream, Named_Id);
      Rose.Objects.Capability_Identifier'Read (Stream, Held_Id);
      Served_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (String'Input (Stream));
      Natural'Read (Stream, Calls);

      if Served_Name = Refused_Name then
         raise Rose.Replacement.Bad_State with
           "Replaceable_Test: refused";
      end if;

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
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Target_Cap = Rose.Capabilities.Null_Capability then
         null;
      elsif Served_Name = Repeat_Name then
         declare
            Target : Rose.Interfaces.Held_Call.Client.Held_Call_Client :=
              Rose.Interfaces.Held_Call.Client.From_Cap (Target_Cap);
            Errors : Natural := 0;
            Value  : Natural := 0;
            Done   : Boolean := False;
         begin
            while not Done loop
               begin
                  Value := Target.Wait_With (Repeat_Name);
                  Done := True;
               exception
                  when others =>
                     Errors := Errors + 1;
               end;
            end loop;
            Waited :=
              Ada.Strings.Unbounded.To_Unbounded_String
                ("waited" & Natural'Image (Value)
                 & " after" & Natural'Image (Errors) & " errors");
         end;
      else
         declare
            Target : Rose.Interfaces.Held_Call.Client.Held_Call_Client :=
              Rose.Interfaces.Held_Call.Client.From_Cap (Target_Cap);
         begin
            Waited :=
              Ada.Strings.Unbounded.To_Unbounded_String
                ("waited" & Natural'Image (Target.Wait));
         exception
            when others =>
               Waited :=
                 Ada.Strings.Unbounded.To_Unbounded_String ("waited error");
         end;
      end if;

      Rose.Server.Start_Server (Context);
   end Start_Server;

   ----------
   -- Wait --
   ----------

   overriding function Wait (Item : in out Held_Server) return Natural is
      pragma Unreferenced (Item);
   begin
      if Answering then
         return Answer_Value;
      end if;

      Rose.Server.Block_Current_Request (Context);
      return 0;
   end Wait;

   ---------------
   -- Wait_With --
   ---------------

   overriding function Wait_With
     (Item : in out Held_Server;
      Text : String)
      return Natural
   is
      pragma Unreferenced (Text);
   begin
      return Wait (Item);
   end Wait_With;

   -------------
   -- Waiting --
   -------------

   overriding function Waiting (Item : in out Held_Server) return Natural is
      pragma Unreferenced (Item);
      use type Rose.Objects.Endpoint_Id;
      Count : Natural := 0;

      procedure Count_Wait
        (Params : in out Rose.Invocation.Invocation_Record;
         Keep   : in out Boolean);

      ----------------
      -- Count_Wait --
      ----------------

      procedure Count_Wait
        (Params : in out Rose.Invocation.Invocation_Record;
         Keep   : in out Boolean)
      is
         pragma Unreferenced (Keep);
      begin
         if Params.Endpoint = Rose.Interfaces.Held_Call.Wait_Endpoint
           or else Params.Endpoint
             = Rose.Interfaces.Held_Call.Wait_With_Endpoint
         then
            Count := Count + 1;
         end if;
      end Count_Wait;

   begin
      Rose.Server.Update_Deferred_Calls (Context, Count_Wait'Access);
      return Count;
   end Waiting;

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
      Rose.Objects.Capability_Identifier'Write
        (Stream,
         Rose.Interfaces.Held_Call.Server.Identifier_Of (The_Held'Access));
      String'Output (Stream, Ada.Strings.Unbounded.To_String (Served_Name));
      Natural'Write (Stream, Calls);
      Named.Add (Service_Name, Service_Cap);
   end Write_State;

end Replaceable_Test_Server;
