with System.Storage_Elements;

package Console.IO is

   procedure Use_Serial_Port;
   --  Write to the serial port, and keep a copy of the last Copy_Size
   --  characters for Replay, instead of writing to the VGA text page,
   --  which isn't on the screen when there is a framebuffer (#654).

   procedure Replay
     (Send : not null access
        procedure (Text : System.Storage_Elements.Storage_Array));
   --  Give Send the copy kept since Use_Serial_Port, a piece at a time,
   --  starting with a line that says so if earlier output was dropped.
   --  Nothing if Use_Serial_Port wasn't called.

   procedure Clear;

   procedure Put (Ch : Character);
   procedure Put (Text : String);
   procedure Put_Line (Text : String);
   procedure New_Line;

end Console.IO;
