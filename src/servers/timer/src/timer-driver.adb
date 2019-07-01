with Timer.Server;

procedure Timer.Driver is
begin
   Timer.Server.Create_Server;
   Timer.Server.Start_Server;
end Timer.Driver;
