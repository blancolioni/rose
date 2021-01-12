with Rose.Console_IO;

with Keyboard.Server;

procedure Keyboard.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Keyboard.Server.Create_Server;
   Keyboard.Server.Start_Server;
end Keyboard.Driver;
