with Rose.Console_IO;

with Timer.Server;

procedure Timer.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Timer.Server.Create_Server;
   Timer.Server.Start_Server;
end Timer.Driver;
