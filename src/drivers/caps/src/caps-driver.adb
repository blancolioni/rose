with Rose.System_Calls;

with Caps.Server;

procedure Caps.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap,
      Delete_Cap      => Delete_Endpoint_Cap,
      Rescind_Cap     => Rescind_Endpoint_Cap);
   Caps.Server.Start_Server;
end Caps.Driver;
