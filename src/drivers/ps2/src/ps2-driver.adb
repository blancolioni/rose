with Rose.System_Calls;

with PS2.Server;

procedure PS2.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   PS2.Server.Create_Server;
   PS2.Server.Start_Server;
end PS2.Driver;
