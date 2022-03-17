with Rose.Console_IO;
with Rose.System_Calls;

with Exec.Server;

procedure Exec.Driver is
begin

   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap,
      Delete_Cap      => Delete_Endpoint_Cap,
      Rescind_Cap     => Rescind_Endpoint_Cap);

   Rose.Console_IO.Open (Console_Cap);
   Exec.Server.Create_Server;
   Exec.Server.Start_Server;
end Exec.Driver;
