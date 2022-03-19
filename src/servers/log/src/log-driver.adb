with Log.Server;

procedure Log.Driver is
begin
   Log.Server.Create_Server;
   Log.Server.Start_Server;
end Log.Driver;
