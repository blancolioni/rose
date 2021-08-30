with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;
with Rose.System_Calls.Client;

with Rose.Interfaces.Timeout.Server;

with Rose.Interfaces.Cap.Client;
with Rose.Interfaces.Timer.Client;

package body Checkpoint.Server is

   Timer : Rose.Interfaces.Timer.Client.Timer_Client;

   Timeout_Cap : Rose.Interfaces.Cap.Client.Cap_Client;

   procedure On_Timeout
     (Id    : in     Rose.Objects.Capability_Identifier);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));

      Console_Cap : constant Rose.Capabilities.Capability := Get_Cap (1);
      Timer_Cap   : constant Rose.Capabilities.Capability := Get_Cap (2);

   begin

      Rose.Console_IO.Open (Console_Cap);
      Rose.Server.Set_Create_Endpoint_Cap (Create_Endpoint_Cap);

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

   ----------------
   -- On_Timeout --
   ----------------

   procedure On_Timeout
     (Id    : in     Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin
      Rose.Console_IO.Put_Line ("running checkpoint ...");
      Rose.Interfaces.Cap.Client.Destroy (Timeout_Cap);
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
