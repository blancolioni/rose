with Rose.Console_IO;

with Exec.Server;

procedure Exec.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Exec.Server.Create_Server;
   Exec.Server.Start_Server;
end Exec.Driver;
