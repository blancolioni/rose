with Rose.Console_IO;

with Bootcaps.Server;

procedure Bootcaps.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Bootcaps.Server.Create_Server;
   Bootcaps.Server.Start_Server;
end Bootcaps.Driver;
