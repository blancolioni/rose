with Rose.Words;

package Console.Calls is

   function Has_Framebuffer return Boolean;
   --  True if the boot loader gave a graphics framebuffer, so that the VGA
   --  text page isn't on the screen (#654).

   procedure Invoke_Memory_Cap;

   procedure Send_Cursor_Position (Position : Rose.Words.Word_16);

   procedure Write_Serial (Text : String);
   --  Write Text to the serial port, COM1.

end Console.Calls;
