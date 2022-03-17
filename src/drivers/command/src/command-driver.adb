with Rose.Console_IO;
with Rose.System_Calls;

with Command.Server;

procedure Command.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Rose.Console_IO.Open (Console_Cap);
   Command.Server.Create_Server;
   Command.Server.Start_Server;
end Command.Driver;
