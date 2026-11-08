with Rose.System_Calls;

with Keyboard.Server;

procedure Keyboard.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Keyboard.Server.Create_Server;
   Keyboard.Server.Start_Server;
end Keyboard.Driver;
