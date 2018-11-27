with Rose.Interfaces.Block_Device.Client;

with Rose.Interfaces.File_System.Client;
with Rose.Interfaces.Directory.Client;

with Rose.Console_IO;

with Rose.Invocation;
with Rose.System_Calls.Server;

with Rose.Devices.Checkpoints;
with Restore.Installer;

procedure Restore.Driver is
   use Rose.Interfaces.Block_Device.Client;
   Device : Block_Device_Client;
begin
   Rose.Console_IO.Open (Console_Cap);

   Rose.Console_IO.Put_Line ("restore: opening swap device");

   Rose.Interfaces.Block_Device.Client.Open_Interface
     (Client         => Device,
      Interface_Cap  => Active_Swap_Cap);

   Rose.Console_IO.Put_Line ("restore: looking for system image");

   if Rose.Devices.Checkpoints.Has_Checkpoint (Device) then
      Rose.Console_IO.Put_Line ("restore: restoring from checkpoint");
   else
      Rose.Console_IO.Put_Line ("restore: creating system image");

      declare
         use Rose.Interfaces.File_System.Client;
         use Rose.Interfaces.Directory.Client;
         File_System : File_System_Client;
         Root        : Directory_Client;
      begin
         Open_Interface (File_System, Install_Media_Cap);
         Root := Root_Directory (File_System);
         Restore.Installer.Install (Root);
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
