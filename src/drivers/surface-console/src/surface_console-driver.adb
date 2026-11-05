with Rose.System_Calls;

with Surface_Console.Server;

procedure Surface_Console.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap,
      Create_Signal   => Create_Signal_Cap);
   Surface_Console.Server.Start_Server;
end Surface_Console.Driver;
