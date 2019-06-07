with Rose.Console_IO;

with Caps.Server;

procedure Caps.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Caps.Server.Create_Server;
   Caps.Server.Start_Server;
end Caps.Driver;
