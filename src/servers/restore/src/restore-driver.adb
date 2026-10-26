with System;

with Rose.Words;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.File_System.Client;

with Rose.Console_IO;
with Rose.Directories;

with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Restore.Checkpoint_Log;
with Restore.Checkpoints;
with Restore.Installer.Default;

procedure Restore.Driver is

   use Rose.Interfaces.Block_Device.Client;
   use type Rose.Words.Word_64;

   Next_Cap_Index : Positive := 1;

   procedure Next (Cap : out Rose.Capabilities.Capability);

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

   Device     : Block_Device_Client;
   Checkpoint : Rose.Words.Word_64;
   Restored   : Boolean := False;

begin

   Next (Delete_Endpoint_Cap);
   Next (Console_Cap);
   Next (Active_Swap_Cap);
   Next (Inactive_Swap_Cap);
   Next (Log_Cap);
   Next (Add_Storage_Cap);
   Next (Reserve_Storage_Cap);
   Next (Write_System_Image);
   Next (Install_Media_Cap);
   Next (Install_Exec_Cap);
   Next (Add_Log_Device_Cap);
   Next (Log_Reader_Cap);
   Next (Restore_Process_Cap);
   Next (Resume_Process_Cap);
   Next (Restore_Interrupt_Cap);
   Next (Store_Restore_Cap);
   Next (Memory_Restore_Cap);
   Next (Log_Reset_Cap);

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

   Rose.Console_IO.Put_Line ("restore: opening swap device");

   Rose.Interfaces.Block_Device.Client.Open
     (Client         => Device,
      Interface_Cap  => Active_Swap_Cap);

   --  The storage server needs its backing store either way: a restore
   --  recreates regions on it, and an install reserves them.

   Rose.Console_IO.Put_Line ("restore: activating storage");

   declare
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Add_Storage_Cap);
      Rose.System_Calls.Send_Cap (Params, Active_Swap_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end;

   if Checkpoint /= 0 then
      Rose.Console_IO.Put_Line ("restore: restoring from checkpoint");
      Restore.Checkpoints.Restore (Log_Reader_Cap, Checkpoint, Restored);
      Rose.System_Calls.Use_Buffer (System.Null_Address, 0);
      if not Restored then
         Rose.Console_IO.Put_Line
           ("restore: cannot restore the checkpoint; installing instead");
      end if;
   end if;

   if not Restored then

      --  A new system starts a new log, so that the entries of the old
      --  one are never mixed with its own.

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Log_Reset_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      Rose.Console_IO.Put_Line ("restore: creating system image");

      declare
         use Rose.Interfaces.File_System.Client;
         File_System : File_System_Client;
      begin
         Open_Cap_Set (File_System, Install_Media_Cap);
         Rose.Directories.Open_Root_File_System (File_System);
         Restore.Installer.Install
           (To     => Device,
            Source => Restore.Installer.Default.Iterate_Steps'Access);
      end;
   end if;

   Rose.Console_IO.Put_Line ("restore: done");

   --  Init's install loop ends with a message without caps.  Its data
   --  word tells init whether the installed programs are running
   --  already, restored from the checkpoint.  After a restore, init
   --  replies once it has started its last boot modules, and only then
   --  can the restored processes run (see Restore.Checkpoints).

   declare
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Install_Exec_Cap);
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

end Restore.Driver;
