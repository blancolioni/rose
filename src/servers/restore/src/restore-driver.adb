with Rose.Interfaces.Block_Device.Client;
with Rose.Devices.GPT;

with Rose.Interfaces.File_System.Client;
with Rose.Interfaces.Directory.Client;

with Rose.Console_IO;

with Rose.Invocation;
with Rose.System_Calls.Server;

with Restore.Installer;

procedure Restore.Driver is
   use Rose.Interfaces.Block_Device.Client;
   Device : Block_Device_Client;
begin
   Rose.Console_IO.Open (Console_Cap);

   Rose.Console_IO.Put_Line ("restore: opening swap device");

   Rose.Interfaces.Block_Device.Client.Open
     (Client         => Device,
      Get_Parameters => Block_Device_Parameters_Cap,
      Read_Blocks    => Block_Device_Read_Cap,
      Write_Blocks   => Block_Device_Write_Cap);

   Rose.Console_IO.Put_Line ("restore: looking for system image");

   if not Rose.Devices.GPT.Has_GPT (Device) then
      Rose.Console_IO.Put_Line ("restore: GPT partition header not found");
      return;
   end if;

   if Rose.Devices.GPT.Partition_Count (Device) = 0 then
      Rose.Console_IO.Put_Line ("restore: no partitions found on device");
      return;
   end if;

   Rose.Console_IO.Put_Line ("Scanning partitions");

   declare
      use Rose.Interfaces.File_System.Client;
      use Rose.Interfaces.Directory.Client;
      File_System : File_System_Client;
      Root        : Directory_Client;
   begin
      Open (File_System, Install_File_System);
      Root := Root_Directory (File_System);
      Restore.Installer.Install (Root);
   end;

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
