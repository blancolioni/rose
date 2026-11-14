with Rose.Capabilities;
with Rose.System_Calls;

with Probe_Server;

--  The main of process_probe, a test server that End_Process_Test
--  launches with Launch (#320, #831).

procedure Process_Probe is
   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   --  From process_probe.driver.
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Probe_Server.Start_Server;
end Process_Probe;
