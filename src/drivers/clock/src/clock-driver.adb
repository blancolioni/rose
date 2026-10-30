with Rose.System_Calls;

with Clock.Server;

procedure Clock.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Clock.Server.Start_Server;
end Clock.Driver;
