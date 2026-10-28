with Rose.System_Calls;

with ATA.Server;

procedure ATA.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   ATA.Server.Create_Server;
   ATA.Server.Start_Server;
end ATA.Driver;
