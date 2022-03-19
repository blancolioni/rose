with Rose.Boot.Console;
with Rose.Invocation;

with Rose.Kernel.Processes;

package body Rose.Kernel.Checkpoint is

   Log_Started : Boolean := False;
   Log_Process : Rose.Kernel.Processes.Process_Id;
   Log_Create  : Rose.Capabilities.Capability := 0;
   Log_Append  : Rose.Capabilities.Capability with Unreferenced;
   Log_Commit  : Rose.Capabilities.Capability := 0;

   procedure Notify_Log
     (Cap      : Rose.Capabilities.Capability;
      Endpoint : Rose.Objects.Endpoint_Id);

   ----------------------
   -- Enter_Checkpoint --
   ----------------------

   procedure Enter_Checkpoint is
   begin
      Rose.Boot.Console.Checkpoint_Status (True);
      Rose.Boot.Console.Put_Line ("Entering checkpoint");
      Rose.Kernel.Processes.Enter_Checkpoint;
      Rose.Boot.Console.Put_Line ("Checkpoint mode ready");

      if Log_Started then
         Notify_Log (Log_Create, 16#08E2_34BA_37F4#);
      end if;
   end Enter_Checkpoint;

   ----------------------
   -- Leave_Checkpoint --
   ----------------------

   procedure Leave_Checkpoint is
   begin
      Rose.Boot.Console.Put_Line ("Leaving checkpoint");
      if Log_Started then
         Notify_Log (Log_Commit, 16#9338_745A_53E1#);
      end if;
      Rose.Kernel.Processes.Leave_Checkpoint;
      Rose.Boot.Console.Checkpoint_Status (False);
   end Leave_Checkpoint;

   ----------------
   -- Notify_Log --
   ----------------

   procedure Notify_Log
     (Cap      : Rose.Capabilities.Capability;
      Endpoint : Rose.Objects.Endpoint_Id)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Control.Flags := (Send       => True,
                               others     => False);

      Params.Cap := Cap;
      Params.Endpoint := Endpoint;
      Params.Identifier := 0;
      Params.Reply_Cap := 0;

      Rose.Kernel.Processes.Send_Cap
        (From_Process_Id => Rose.Kernel.Processes.To_Process_Id (1),
         To_Process_Id   => Log_Process,
         Sender_Cap      => 0,
         Receiver_Cap    => Cap,
         Params          => Params);

      Rose.Kernel.Processes.Set_Current_State
        (Log_Process,
         Rose.Kernel.Processes.Ready);

   end Notify_Log;

   ----------------------
   -- Set_Log_Handlers --
   ----------------------

   procedure Set_Log_Handlers
     (Create, Append, Commit : Rose.Capabilities.Capability)
   is
   begin
      Log_Process := Rose.Kernel.Processes.Current_Process_Id;
      Log_Create := Create;
      Log_Append := Append;
      Log_Commit := Commit;
      Log_Started := True;
   end Set_Log_Handlers;

end Rose.Kernel.Checkpoint;
