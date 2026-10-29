with System.Storage_Elements;

with Rose.Words;

package VGA_Console.Line_Discipline is

   --  The console's line discipline (#397, #454). In cooked mode, it
   --  echoes what is typed, acts on control characters, and a Read
   --  returns only whole lines. In raw mode, every character is data,
   --  and nothing is echoed.

   procedure On_Key
     (Code  : Rose.Words.Word;
      Ready : out Boolean);
   --  Take in a character from the keyboard. Ready is True if a Read
   --  that is waiting can now go ahead.

   function Ready return Boolean;
   --  True if there are whole lines to read, or an end of file.

   procedure Take
     (Buffer : out System.Storage_Elements.Storage_Array;
      Last   : out System.Storage_Elements.Storage_Count);
   --  Take the whole lines into Buffer, as many as fit, or an end of file
   --  as nothing (Last < Buffer'First). Only called when Ready.

   procedure Set_Echo (Enabled : Boolean);
   --  Echo what is typed, or not.

end VGA_Console.Line_Discipline;
