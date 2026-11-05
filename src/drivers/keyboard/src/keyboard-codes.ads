with Rose.Keys;
with Rose.Words;

package Keyboard.Codes is

   type Key_Press is
      record
         Key       : Rose.Keys.Key_Code;
         Modifiers : Rose.Keys.Modifier_Set;
      end record;

   type Key_Press_Array is array (Positive range <>) of Key_Press;

   procedure Handle_Key
     (Scan_Code : Rose.Words.Word_8;
      Result    : out Key_Press_Array;
      Last      : out Natural)
     with Pre => Result'First = 1 and then Result'Length >= 2;
   --  Scan_Code is a byte from the keyboard controller, in scan code set
   --  1. Result (1 .. Last) are the keys it gives, with the modifiers
   --  that are held, which may be none (#734). An E0 prefix selects the
   --  extended keys for the next byte (#397). The layout's dead keys give
   --  nothing, and the key after one gives the character that the two
   --  compose, or both if they don't.

end Keyboard.Codes;
