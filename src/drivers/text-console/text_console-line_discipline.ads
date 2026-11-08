with Ada.Streams;

with Rose.Keys;

generic
   with procedure Put (Text : String);
   --  Write Text to the console's screen: the echo of what is typed.
package Text_Console.Line_Discipline is

   --  The console's line discipline (#397, #454, #734). With line editing
   --  on, it echoes what is typed, acts on control characters, and a Read
   --  returns only whole lines. With line editing off, it keeps the keys
   --  for Read_Key, and nothing is echoed.

   procedure On_Key
     (Key       : Rose.Keys.Key_Code;
      Modifiers : Rose.Keys.Modifier_Set);
   --  Take in a key from the keyboard.

   function Ready return Boolean;
   --  True if a Read can go ahead: with line editing on, if there are
   --  whole lines to read, or an end of file, and with it off, if a key
   --  that is an ASCII character has been typed.

   procedure Take
     (Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
     with Pre => Ready;
   --  With line editing on, take the whole lines into Buffer, as many as
   --  fit, or an end of file as nothing (Last < Buffer'First). With it
   --  off, take the keys that have been typed, as many as fit, and give
   --  the ASCII characters among them.

   function Key_Ready return Boolean;
   --  True if line editing is off, and a key has been typed.

   procedure Take_Key
     (Key       : out Rose.Keys.Key_Code;
      Modifiers : out Rose.Keys.Modifier_Set)
     with Pre => Key_Ready;
   --  Take the first key that was typed.

   function Editing return Boolean;

   procedure Set_Editing (Enabled : Boolean);
   --  Turn line editing on or off. Keys that haven't been read are
   --  dropped when it is turned on, and the line being typed when it is
   --  turned off.

   procedure Set_Echo (Enabled : Boolean);
   --  Echo what is typed, with line editing on, or not.

end Text_Console.Line_Discipline;
