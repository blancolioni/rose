with Rose.Build;

with Console.Calls;
with Console.IO;
with Console.Server;

procedure Console.Driver is
begin

   if Console.Calls.Has_Framebuffer then
      Console.IO.Use_Serial_Port;
   else
      Console.Calls.Invoke_Memory_Cap;
      if Console.Calls.Has_Serial_Console then
         Console.IO.Mirror_Serial_Port;
      end if;
   end if;

   Console.IO.Clear;
   Console.IO.Put_Line (Rose.Build.Full_Name);
   Console.Server.Create_Console_Server;
   Console.Server.Start_Server;
end Console.Driver;
