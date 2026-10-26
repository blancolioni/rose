with Rose.System_Calls;

with Directory.Server;

procedure Directory.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Directory.Server.Create_Server;
   Directory.Server.Start_Server;
end Directory.Driver;
