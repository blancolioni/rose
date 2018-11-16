with Rose.Console_IO;
with PCI.Server;

procedure PCI.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);

   Rose.Console_IO.Put_Line ("pci: scanning bus");
   PCI.Server.Scan_Devices;

   Rose.Console_IO.Put_Line ("pci: starting server ...");
   PCI.Server.Start_Server;
end PCI.Driver;
