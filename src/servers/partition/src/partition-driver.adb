with Partition.Server;

procedure Partition.Driver is
begin
   Partition.Server.Create_Server;
   Partition.Server.Start_Server;
end Partition.Driver;
