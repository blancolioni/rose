with Rose.Console_IO;

with Mem.Server;

procedure Mem.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Mem.Server.Create_Server;
   Mem.Server.Start_Server;
end Mem.Driver;
