with Rose.Words;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.File_System.Client;

with Rose.Console_IO;
with Rose.Directories;

with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Restore.Checkpoint_Log;
with Restore.Installer.Default;

procedure Restore.Driver is

   use Rose.Interfaces.Block_Device.Client;
   use type Rose.Words.Word_64;

   Restore_Enabled : constant Boolean := False;
   --  Restoring from a checkpoint is not implemented yet (#102), so a
   --  checkpoint is found and reported, but the system is installed
   --  as before.

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

   if Checkpoint /= 0 and then not Restore_Enabled then
      Rose.Console_IO.Put_Line
        ("restore: restoring from a checkpoint is not implemented yet");
   end if;

   if Checkpoint /= 0 and then Restore_Enabled then
      Rose.Console_IO.Put_Line ("restore: restoring from checkpoint");
   else
      Rose.Console_IO.Put_Line ("restore: activating storage");

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Add_Storage_Cap);
         Rose.System_Calls.Send_Cap (Params, Active_Swap_Cap);
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

   declare
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Install_Exec_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end;

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
