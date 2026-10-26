with Rose.System_Calls;

with VGA_Console.Server;

procedure VGA_Console.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   VGA_Console.Server.Start_Server;
end VGA_Console.Driver;
