with Rose.System_Calls;

with Screen.Server;

procedure Screen.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Screen.Server.Start_Server;
end Screen.Driver;
