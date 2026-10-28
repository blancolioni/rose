with Rose.System_Calls;

with PCI.Server;

procedure PCI.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   PCI.Server.Create_Caps;
   PCI.Server.Scan_Devices;
   PCI.Server.Start_Server;
end PCI.Driver;
