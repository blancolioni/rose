with Keyboard.Server;

procedure Keyboard.Driver is
begin
   Keyboard.Server.Create_Server;
   Keyboard.Server.Start_Server;
end Keyboard.Driver;
