with Rose.Version;

with Console.Calls;
with Console.IO;
with Console.Server;

procedure Console.Driver is
begin

   Console.Calls.Invoke_Memory_Cap;

   if False then
      for I in 1 .. 16 loop
         Console.IO.New_Line;
      end loop;
   else
      Console.IO.Clear;
   end if;
   Console.IO.Put_Line (Rose.Version.Full_Name);
   Console.Server.Create_Console_Server;
   Console.Server.Start_Server;
end Console.Driver;
