with Rose.Capabilities;
with Rose.System_Calls;

with Memory_Surface_Server;

--  The main of memory_surface, a test-only stand-in for the screen: a
--  Test_Surface in plain memory, which the display server's tests launch
--  with Launch and draw on (#583, ADR 0022 section 1).

procedure Memory_Surface is
   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   --  From memory_surface.driver.
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Memory_Surface_Server.Start_Server;
end Memory_Surface;
