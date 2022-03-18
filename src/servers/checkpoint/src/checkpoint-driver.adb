with Checkpoint.Server;

procedure Checkpoint.Driver is
begin
   Checkpoint.Server.Create_Server;
   Checkpoint.Server.Start_Server;
end Checkpoint.Driver;
