with Rose.Interfaces.Block_Device.Client;

with Rose.Interfaces.File_System.Client;

with Rose.Console_IO;
with Rose.Directories;

with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Rose.Devices.Checkpoints;
with Restore.Installer;

procedure Restore.Driver is
   use Rose.Interfaces.Block_Device.Client;

   procedure Next (Cap : out Rose.Capabilities.Capability);

   ----------
   -- Next --
   ----------

   procedure Next (Cap : out Rose.Capabilities.Capability) is
   begin
      Cap := Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
   end Next;

   Device : Block_Device_Client;

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

   Rose.Console_IO.Open (Console_Cap);

   Rose.Console_IO.Put_Line ("restore: opening swap device");

   Rose.Interfaces.Block_Device.Client.Open
     (Client         => Device,
      Interface_Cap  => Active_Swap_Cap);

   Rose.Console_IO.Put_Line ("restore: looking for system image");

   if Rose.Devices.Checkpoints.Has_Checkpoint (Device) then
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
         Restore.Installer.Install (Device);
      end;
   end if;

   Rose.Console_IO.Put_Line ("restore: done");

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
