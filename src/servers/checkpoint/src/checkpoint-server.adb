with Rose.Invocation;
with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;
with Rose.System_Calls.Client;

with Rose.Interfaces.Timeout.Server;

with Rose.Interfaces.Cap.Client;
with Rose.Interfaces.Timer.Client;

package body Checkpoint.Server is

   Timer : Rose.Interfaces.Timer.Client.Timer_Client;

   Timeout_Cap           : Rose.Interfaces.Cap.Client.Cap_Client;
   Enter_Checkpoint_Cap  : Rose.Capabilities.Capability;
   Leave_Checkpoint_Cap  : Rose.Capabilities.Capability;
   Memory_Checkpoint_Cap : Rose.Capabilities.Capability;
   Append_Log_Cap        : Rose.Capabilities.Capability;

   procedure On_Timeout
     (Id    : in     Rose.Objects.Capability_Identifier);

   procedure Enter_Checkpoint;
   procedure Leave_Checkpoint;

   procedure Execute_Checkpoint;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));

      Console_Cap : constant Rose.Capabilities.Capability := Get_Cap (1);
      Timer_Cap   : constant Rose.Capabilities.Capability := Get_Cap (2);
      Delete_Cap  : constant Rose.Capabilities.Capability := Get_Cap (5);
   begin

      Enter_Checkpoint_Cap := Get_Cap (3);
      Leave_Checkpoint_Cap := Get_Cap (4);
      Memory_Checkpoint_Cap := Get_Cap (6);
      Append_Log_Cap := Get_Cap (7);

      Rose.Console_IO.Open (Console_Cap);
      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap,
         Delete_Cap => Delete_Cap);

      Rose.Interfaces.Timer.Client.Open_Cap_Set
        (Client    => Timer,
         Set_Timer => Timer_Cap);

      Rose.Interfaces.Timeout.Server.Create_Server
        (Server_Context => Server_Context,
         On_Timeout     => On_Timeout'Access);

      Timeout_Cap :=
        Rose.Interfaces.Timer.Client.Set_Timer
          (Item         => Timer,
           Milliseconds => 30_000,
           Cap          => Rose.Interfaces.Timeout.Server.Get_On_Timeout_Cap);

   end Create_Server;

   ----------------------
   -- Enter_Checkpoint --
   ----------------------

   procedure Enter_Checkpoint is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Enter_Checkpoint_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Enter_Checkpoint;

   ------------------------
   -- Execute_Checkpoint --
   ------------------------

   procedure Execute_Checkpoint is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Memory_Checkpoint_Cap);
      Rose.System_Calls.Send_Cap (Params, Append_Log_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Execute_Checkpoint;

   ----------------------
   -- Leave_Checkpoint --
   ----------------------

   procedure Leave_Checkpoint is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Leave_Checkpoint_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Leave_Checkpoint;

   ----------------
   -- On_Timeout --
   ----------------

   procedure On_Timeout
     (Id    : in     Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin
      Rose.Interfaces.Cap.Client.Destroy (Timeout_Cap);
      Rose.Interfaces.Cap.Client.Close (Timeout_Cap);

      Enter_Checkpoint;
      Execute_Checkpoint;
      Leave_Checkpoint;

      Timeout_Cap :=
        Rose.Interfaces.Timer.Client.Set_Timer
          (Item         => Timer,
           Milliseconds => 30_000,
           Cap          => Rose.Interfaces.Timeout.Server.Get_On_Timeout_Cap);
   end On_Timeout;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Console_IO.Put_Line ("checkpoint: starting server");
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end Checkpoint.Server;
