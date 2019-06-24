with Mem.Server;

procedure Mem.Driver is
begin
   Mem.Server.Create_Server;
   Mem.Server.Start_Server;
end Mem.Driver;
