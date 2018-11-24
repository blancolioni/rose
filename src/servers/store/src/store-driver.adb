with Rose.Console_IO;

with Store.Server;

procedure Store.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Store.Server.Create_Server;
   Store.Server.Start_Server;
end Store.Driver;
