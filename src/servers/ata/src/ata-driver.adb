with Rose.Console_IO;
with ATA.Server;

procedure ATA.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Rose.Console_IO.Put_Line ("ata: initialising");
   ATA.Server.Create_Server;
   Rose.Console_IO.Put_Line ("ata: starting server");
   ATA.Server.Start_Server;
end ATA.Driver;
