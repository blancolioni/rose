with Rose.Console_IO;
with Rose.Invocation;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Cap.Server;
with Rose.Interfaces.Timeout.Server;
with Rose.Interfaces.Timer.Server;

with Rose.Server;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

package body Timer.Server is

   type Ticks_Type is new Rose.Words.Word;

   type Timeout_Entry is
      record
         Id      : Rose.Objects.Capability_Identifier;
         Timeout : Ticks_Type;
         Cap     : Rose.Capabilities.Capability;
      end record;

   Max_Timer_Count : constant := 100;
   type Timer_Count is range 0 .. Max_Timer_Count;
   subtype Timer_Index is Timer_Count range 1 .. Timer_Count'Last;

   Queue       : array (Timer_Index) of Timeout_Entry;
   Queue_Front  : Timer_Count := 0;
   Server      : Rose.Server.Server_Context;
   Timeout_Cap : Rose.Capabilities.Capability;
   Next_Cap_Id : Rose.Objects.Capability_Identifier := 1;

   function Next_Id return Rose.Objects.Capability_Identifier;

   procedure On_Timeout (Id : Rose.Objects.Capability_Identifier);
   function Set_Timer
     (Id           : Rose.Objects.Capability_Identifier;
      Milliseconds : Rose.Words.Word;
      Cap          : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   procedure Destroy_Timer (Id : Rose.Objects.Capability_Identifier);
   function Get_Timer_Object_Id (Id : Rose.Objects.Capability_Identifier)
     return Rose.Objects.Object_Id;

   procedure Set_Timer (Ticks : Ticks_Type);

   procedure Update_Timer_Queue;

   function Current_Ticks return Ticks_Type;
   function To_Ticks (Milliseconds : Rose.Words.Word)
                      return Ticks_Type
   is (Ticks_Type (Rose.Words."/" (Milliseconds, 10)));

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin

      Console_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
      Rose.Console_IO.Open (Console_Cap);
      Rose.Console_IO.Put_Line ("timer: creating server");

      Set_Timeout_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
      Get_Current_Ticks_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Timeout_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Endpoint_Cap,
           Rose.Interfaces.Timeout.On_Timeout_Endpoint);

      Rose.Interfaces.Timer.Server.Create_Server
        (Server, Set_Timer'Access);
      Rose.Interfaces.Timeout.Server.Attach_Interface
        (Server, On_Timeout'Access, Instanced => True);
      Rose.Interfaces.Cap.Server.Attach_Interface
        (Server, Destroy_Timer'Access, Get_Timer_Object_Id'Access,
         Instanced => True);
   end Create_Server;

   -------------------
   -- Current_Ticks --
   -------------------

   function Current_Ticks return Ticks_Type is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Get_Current_Ticks_Cap);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      return Ticks_Type (Params.Data (0));
   end Current_Ticks;

   -------------------
   -- Destroy_Timer --
   -------------------

   procedure Destroy_Timer (Id : Rose.Objects.Capability_Identifier) is
      use type Rose.Objects.Capability_Identifier;
      Found : Boolean := False;
   begin
      for Index in 1 .. Queue_Front loop
         if Queue (Index).Id = Id then
            Found := True;
         elsif Found then
            Queue (Index - 1) := Queue (Index);
         end if;
      end loop;

      if Found then
         Queue_Front := Queue_Front - 1;
      end if;

   end Destroy_Timer;

   -------------------------
   -- Get_Timer_Object_Id --
   -------------------------

   function Get_Timer_Object_Id
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id
   is
      pragma Unreferenced (Id);
   begin
      return 0;
   end Get_Timer_Object_Id;

   -------------
   -- Next_Id --
   -------------

   function Next_Id return Rose.Objects.Capability_Identifier is
      use type Rose.Objects.Capability_Identifier;
   begin
      return Id : constant Rose.Objects.Capability_Identifier := Next_Cap_Id
      do
         Next_Cap_Id := Next_Cap_Id + 1;
      end return;
   end Next_Id;

   ----------------
   -- On_Timeout --
   ----------------

   procedure On_Timeout (Id : Rose.Objects.Capability_Identifier) is
      pragma Unreferenced (Id);
      Params : aliased Rose.Invocation.Invocation_Record;
      Timeout : constant Timeout_Entry :=
                  Queue (Queue_Front);
   begin
      Rose.System_Calls.Initialize_Send (Params, Timeout.Cap);
      Params.Control.Flags (Rose.Invocation.Block) := False;
      Rose.System_Calls.Invoke_Capability (Params);
      Queue_Front := Queue_Front - 1;
      Update_Timer_Queue;
   end On_Timeout;

   ---------------
   -- Set_Timer --
   ---------------

   function Set_Timer
     (Id           : Rose.Objects.Capability_Identifier;
      Milliseconds : Rose.Words.Word;
      Cap          : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      New_Entry : constant Timeout_Entry :=
                    Timeout_Entry'
                      (Id      => Next_Id,
                       Timeout => To_Ticks (Milliseconds) + Current_Ticks,
                       Cap     => Cap);
      Index     : Timer_Count := Queue_Front;
      Reset     : constant Boolean :=
                    (Queue_Front = 0
                     or else New_Entry.Timeout
                     < Queue (Queue_Front).Timeout);
   begin
      while Index > 0
        and then Queue (Index).Timeout < New_Entry.Timeout
      loop
         Queue (Index + 1) := Queue (Index);
         Index := Index - 1;
      end loop;
      Queue (Index + 1) := New_Entry;
      Queue_Front := Queue_Front + 1;

      if Reset then
         Update_Timer_Queue;
      end if;

      declare
         Result_Cap : constant Rose.Capabilities.Capability :=
                        Rose.System_Calls.Server.Create_Endpoint
                          (Create_Cap  => Create_Endpoint_Cap,
                           Endpoint_Id => Rose.Interfaces.Cap.Cap_Interface,
                           Identifier  => New_Entry.Id);
      begin
         return Result_Cap;
      end;

   end Set_Timer;

   ---------------
   -- Set_Timer --
   ---------------

   procedure Set_Timer (Ticks : Ticks_Type) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Set_Timeout_Cap);
      Rose.System_Calls.Send_Word
        (Params, Rose.Words.Word (Ticks));
      Rose.System_Calls.Send_Cap
        (Params, Timeout_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Set_Timer;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Server);
   end Start_Server;

   ------------------------
   -- Update_Timer_Queue --
   ------------------------

   procedure Update_Timer_Queue is
   begin
      if Queue_Front > 0 then
         declare
            Ticks : constant Ticks_Type :=
                      Queue (Queue_Front).Timeout;
            Current : constant Ticks_Type := Current_Ticks;
         begin
            Set_Timer
              (if Ticks < Current then 0 else Ticks - Current);
         end;
      end if;
   end Update_Timer_Queue;

end Timer.Server;
