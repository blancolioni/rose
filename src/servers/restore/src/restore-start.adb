with System;

with Rose.Console_IO;

with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Restore.Checkpoint_Log;
with Restore.Checkpoints;

package body Restore.Start is

   Next_Cap_Index : Positive := 1;

   ------------
   -- Finish --
   ------------

   procedure Finish (Restored : Boolean) is
   begin
      Rose.Console_IO.Put_Line ("restore: done");

      --  Init waits for a message without caps.  Its data word tells init
      --  whether the installed programs are running already, restored
      --  from the checkpoint.  After a restore, init replies once it has
      --  started its last boot modules, and only then can the restored
      --  processes run (see Restore.Checkpoints).

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Init_Cap);
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word'(if Restored then 1 else 0));
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      if Restored then
         Restore.Checkpoints.Resume_Processes;
         Rose.Console_IO.Put_Line ("restore: restored processes are running");
      end if;

      declare
         use Rose.Invocation;
         use Rose.System_Calls;
         Params : aliased Rose.Invocation.Invocation_Record;
         Receive_Cap : constant Rose.Capabilities.Capability :=
                         Rose.System_Calls.Server.Create_Receive_Cap
                           (Create_Cap   => Create_Endpoint_Cap);
      begin
         Params :=
           Invocation_Record'
             (Control       =>
                Control_Word'
                  (Flags          =>
                     (Receive     => True,
                      Block       => True,
                      Recv_Words  => True,
                      Recv_Caps   => True,
                      others      => False),
                   others         => <>),
              Cap           => Receive_Cap,
              others        => <>);

         Invoke_Capability (Params);
      end;
   end Finish;

   --------------
   -- Get_Caps --
   --------------

   procedure Get_Caps is
   begin
      Next (Console_Cap);
      Next (Swap_Cap);
      Next (Log_Cap);
      Next (Add_Storage_Cap);
      Next (Init_Cap);
      Next (Add_Log_Device_Cap);
      Next (Log_Reader_Cap);
      Next (Restore_Process_Cap);
      Next (Resume_Process_Cap);
      Next (Restore_Interrupt_Cap);
      Next (Store_Restore_Cap);
      Next (Memory_Restore_Cap);
   end Get_Caps;

   ----------
   -- Next --
   ----------

   procedure Next (Cap : out Rose.Capabilities.Capability) is
   begin
      Cap :=
        Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Next_Cap_Index)));
      Next_Cap_Index := Next_Cap_Index + 1;
   end Next;

   -------------
   -- Prepare --
   -------------

   procedure Prepare (Checkpoint : out Rose.Words.Word_64) is
   begin
      Rose.Console_IO.Open (Console_Cap);

      --  The log server can only read the log once it has its device.

      Rose.Console_IO.Put_Line ("restore: adding log device");

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Add_Log_Device_Cap);
         Rose.System_Calls.Send_Cap (Params, Log_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      --  A checkpoint is the last complete entry in the checkpoint log.

      Checkpoint := Restore.Checkpoint_Log.Find_Checkpoint (Log_Reader_Cap);

      --  The storage server needs its backing store either way: a restore
      --  recreates regions on it, and an install reserves them.

      Rose.Console_IO.Put_Line ("restore: activating storage");

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Add_Storage_Cap);
         Rose.System_Calls.Send_Cap (Params, Swap_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;
   end Prepare;

   ------------------------
   -- Restore_Checkpoint --
   ------------------------

   procedure Restore_Checkpoint
     (Checkpoint : Rose.Words.Word_64;
      Restored   : out Boolean)
   is
   begin
      Rose.Console_IO.Put_Line ("restore: restoring from checkpoint");
      Restore.Checkpoints.Restore (Log_Reader_Cap, Checkpoint, Restored);
      Rose.System_Calls.Use_Buffer (System.Null_Address, 0);
      if not Restored then
         Rose.Console_IO.Put_Line ("restore: cannot restore the checkpoint");
      end if;
   end Restore_Checkpoint;

end Restore.Start;
