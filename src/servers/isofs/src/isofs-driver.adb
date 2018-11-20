with Rose.Console_IO;

with IsoFS.Server;

procedure IsoFS.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   IsoFS.Server.Start_Server;
end IsoFS.Driver;
