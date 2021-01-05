with Rose.Console_IO;

with Command.Server;

procedure Command.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Rose.Console_IO.Put_Line ("command: starting");
   Command.Server.Create_Server;
   Command.Server.Start_Server;
end Command.Driver;
