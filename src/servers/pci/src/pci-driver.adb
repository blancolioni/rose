with PCI.Server;

procedure PCI.Driver is
begin
   PCI.Server.Create_Caps;
   PCI.Server.Scan_Devices;
   PCI.Server.Start_Server;
end PCI.Driver;
