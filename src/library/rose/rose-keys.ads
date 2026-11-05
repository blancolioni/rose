with Rose.Words;

package Rose.Keys with Pure is

   --  The keys that the keyboard gives, and Terminal.Read_Key returns
   --  (#734). A key is the Unicode code point of the character that it
   --  types, or a named key, numbered above the code points. Return is
   --  a line feed, Backspace is 8, Tab is 9 and Escape is 27, and a
   --  letter typed with Control is its control character.

   use type Rose.Words.Word;

   subtype Key_Code is Rose.Words.Word range 0 .. 16#11_00FF#;

   Last_Code_Point : constant Key_Code := 16#10_FFFF#;
   First_Named_Key : constant Key_Code := 16#11_0000#;

   Up        : constant Key_Code := First_Named_Key;
   Down      : constant Key_Code := First_Named_Key + 1;
   Left      : constant Key_Code := First_Named_Key + 2;
   Right     : constant Key_Code := First_Named_Key + 3;
   Home      : constant Key_Code := First_Named_Key + 4;
   End_Key   : constant Key_Code := First_Named_Key + 5;
   Page_Up   : constant Key_Code := First_Named_Key + 6;
   Page_Down : constant Key_Code := First_Named_Key + 7;
   Insert    : constant Key_Code := First_Named_Key + 8;
   Delete    : constant Key_Code := First_Named_Key + 9;

   F1        : constant Key_Code := First_Named_Key + 16#10#;
   --  F1 to F12 are F1 + 0 to F1 + 11.

   function Function_Key (N : Positive) return Key_Code
   is (F1 + Rose.Words.Word (N) - 1)
     with Pre => N <= 12;

   function Is_Named (Key : Key_Code) return Boolean
   is (Key >= First_Named_Key);

   --  The modifiers: a mask of the modifier keys that were held. AltGr
   --  is not one of them, since it chooses the character that a key
   --  types; on a layout without AltGr characters, it is Alt.

   subtype Modifier_Set is Rose.Words.Word range 0 .. 7;

   No_Modifiers : constant Modifier_Set := 0;
   Shift        : constant Modifier_Set := 1;
   Control      : constant Modifier_Set := 2;
   Alt          : constant Modifier_Set := 4;

   --  A key and its modifiers in one word, as the keyboard gives them to
   --  its listener: the key in the low 24 bits, and the modifiers above.

   function Pack
     (Key       : Key_Code;
      Modifiers : Modifier_Set)
      return Rose.Words.Word
   is (Key + Modifiers * 2 ** 24);

   function Key_Of (Packed : Rose.Words.Word) return Key_Code
   is (Packed mod 2 ** 24)
     with Pre => Packed mod 2 ** 24 <= Key_Code'Last;

   function Modifiers_Of (Packed : Rose.Words.Word) return Modifier_Set
   is (Packed / 2 ** 24 mod 8);

end Rose.Keys;
