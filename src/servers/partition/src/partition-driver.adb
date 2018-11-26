with Rose.Console_IO;

with Partition.Server;

procedure Partition.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Partition.Server.Create_Server;
   Partition.Server.Start_Server;
end Partition.Driver;
