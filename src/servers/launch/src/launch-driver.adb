with Rose.Console_IO;

with Launch.Server;

procedure Launch.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Launch.Server.Create_Server;
   Launch.Server.Start_Server;
end Launch.Driver;
