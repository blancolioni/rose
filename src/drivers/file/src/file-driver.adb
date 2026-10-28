with Rose.System_Calls;

with File.Server;

procedure File.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap,
      Delete_Cap      => Delete_Cap_Cap);
   File.Server.Create_Server;
   File.Server.Start_Server;
end File.Driver;
