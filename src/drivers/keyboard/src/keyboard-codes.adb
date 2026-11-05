package body Keyboard.Codes is

   use Rose.Keys;
   use type Rose.Words.Word;
   use type Rose.Words.Word_8;

   --  The layout: what each key gives (#734). A character key has four
   --  levels, chosen by Shift and AltGr, and a level can be a dead key,
   --  which gives the accent that the next key is composed with. Only
   --  the US layout exists, which has no AltGr level and no dead keys.

   type Level is (Base, Shifted, AltGr, Shifted_AltGr);

   type Level_Keys is array (Level) of Key_Code;
   --  0 for a level that gives nothing.

   type Level_Flags is array (Level) of Boolean;

   type Modifier_Key is (Shift_Key, Control_Key, Alt_Key, AltGr_Key);

   type Key_Kind is (No_Key, Modifier_Kind, Character_Kind, Named_Kind);

   type Key_Entry is
      record
         Kind      : Key_Kind := No_Key;
         Held      : Modifier_Key := Shift_Key;
         Values    : Level_Keys := (others => 0);
         Dead      : Level_Flags := (others => False);
         Named_Key : Key_Code := 0;
      end record;

   function Modifier (M : Modifier_Key) return Key_Entry
   is ((Kind => Modifier_Kind, Held => M, others => <>));

   function Name (Key : Key_Code) return Key_Entry
   is ((Kind => Named_Kind, Named_Key => Key, others => <>));

   function Fn (N : Positive) return Key_Entry
   is (Name (Function_Key (N)));

   function Asc
     (Base_Value    : Character;
      Shifted_Value : Character)
      return Key_Entry
   is ((Kind   => Character_Kind,
        Values => (Character'Pos (Base_Value),
                   Character'Pos (Shifted_Value),
                   0, 0),
        others => <>));

   function Asc (Value : Character) return Key_Entry
   is (Asc (Value, Value));

   ESC : constant Character := Character'Val (27);
   BS  : constant Character := Character'Val (8);
   HT  : constant Character := Character'Val (9);
   LF  : constant Character := Character'Val (10);

   Key_Code_Table : constant array (Rose.Words.Word_8) of Key_Entry :=
     (16#2A# => Modifier (Shift_Key),
      16#36# => Modifier (Shift_Key),
      16#1D# => Modifier (Control_Key),
      16#38# => Modifier (Alt_Key),
      16#39# => Asc (' '),
      16#3B# => Fn (1),
      16#3C# => Fn (2),
      16#3D# => Fn (3),
      16#3E# => Fn (4),
      16#3F# => Fn (5),
      16#40# => Fn (6),
      16#41# => Fn (7),
      16#42# => Fn (8),
      16#43# => Fn (9),
      16#44# => Fn (10),
      16#57# => Fn (11),
      16#58# => Fn (12),
      16#01# => Asc (ESC),
      16#02# => Asc ('1', '!'),
      16#03# => Asc ('2', '@'),
      16#04# => Asc ('3', '#'),
      16#05# => Asc ('4', '$'),
      16#06# => Asc ('5', '%'),
      16#07# => Asc ('6', '^'),
      16#08# => Asc ('7', '&'),
      16#09# => Asc ('8', '*'),
      16#0A# => Asc ('9', '('),
      16#0B# => Asc ('0', ')'),
      16#0C# => Asc ('-', '_'),
      16#0D# => Asc ('=', '+'),
      16#0E# => Asc (BS),
      16#0F# => Asc (HT),
      16#10# => Asc ('q', 'Q'),
      16#11# => Asc ('w', 'W'),
      16#12# => Asc ('e', 'E'),
      16#13# => Asc ('r', 'R'),
      16#14# => Asc ('t', 'T'),
      16#15# => Asc ('y', 'Y'),
      16#16# => Asc ('u', 'U'),
      16#17# => Asc ('i', 'I'),
      16#18# => Asc ('o', 'O'),
      16#19# => Asc ('p', 'P'),
      16#1A# => Asc ('[', '{'),
      16#1B# => Asc (']', '}'),
      16#1C# => Asc (LF),
      16#1E# => Asc ('a', 'A'),
      16#1F# => Asc ('s', 'S'),
      16#20# => Asc ('d', 'D'),
      16#21# => Asc ('f', 'F'),
      16#22# => Asc ('g', 'G'),
      16#23# => Asc ('h', 'H'),
      16#24# => Asc ('j', 'J'),
      16#25# => Asc ('k', 'K'),
      16#26# => Asc ('l', 'L'),
      16#27# => Asc (';', ':'),
      16#28# => Asc (''', '"'),
      16#29# => Asc ('`', '~'),
      16#2B# => Asc ('\', '|'),
      16#2C# => Asc ('z', 'Z'),
      16#2D# => Asc ('x', 'X'),
      16#2E# => Asc ('c', 'C'),
      16#2F# => Asc ('v', 'V'),
      16#30# => Asc ('b', 'B'),
      16#31# => Asc ('n', 'N'),
      16#32# => Asc ('m', 'M'),
      16#33# => Asc (',', '<'),
      16#34# => Asc ('.', '>'),
      16#35# => Asc ('/', '?'),
      others => <>);

   --  The keys that follow an E0 prefix. Right Control is Control, and
   --  right Alt is AltGr.

   Extended_Key_Code_Table : constant array (Rose.Words.Word_8)
     of Key_Entry :=
       (16#1D# => Modifier (Control_Key),
        16#38# => Modifier (AltGr_Key),
        16#1C# => Asc (LF),
        16#35# => Asc ('/'),
        16#47# => Name (Home),
        16#48# => Name (Up),
        16#49# => Name (Page_Up),
        16#4B# => Name (Left),
        16#4D# => Name (Right),
        16#4F# => Name (End_Key),
        16#50# => Name (Down),
        16#51# => Name (Page_Down),
        16#52# => Name (Insert),
        16#53# => Name (Delete),
        others => <>);

   --  The characters that a dead key's accent composes with a letter:
   --  the capital letter and its accented form in Latin-1. The small
   --  letters are 32 above them.

   type Composition is
      record
         Accent, Letter, Composed : Key_Code;
      end record;

   Grave      : constant Key_Code := 16#60#;
   Acute      : constant Key_Code := 16#B4#;
   Circumflex : constant Key_Code := 16#5E#;
   Tilde      : constant Key_Code := 16#7E#;
   Diaeresis  : constant Key_Code := 16#A8#;
   Cedilla    : constant Key_Code := 16#B8#;

   function L (Letter : Character) return Key_Code
   is (Character'Pos (Letter));

   Compositions : constant array (Positive range <>) of Composition :=
     ((Grave, L ('A'), 16#C0#), (Grave, L ('E'), 16#C8#),
      (Grave, L ('I'), 16#CC#), (Grave, L ('O'), 16#D2#),
      (Grave, L ('U'), 16#D9#),
      (Acute, L ('A'), 16#C1#), (Acute, L ('E'), 16#C9#),
      (Acute, L ('I'), 16#CD#), (Acute, L ('O'), 16#D3#),
      (Acute, L ('U'), 16#DA#), (Acute, L ('Y'), 16#DD#),
      (Circumflex, L ('A'), 16#C2#), (Circumflex, L ('E'), 16#CA#),
      (Circumflex, L ('I'), 16#CE#), (Circumflex, L ('O'), 16#D4#),
      (Circumflex, L ('U'), 16#DB#),
      (Tilde, L ('A'), 16#C3#), (Tilde, L ('N'), 16#D1#),
      (Tilde, L ('O'), 16#D5#),
      (Diaeresis, L ('A'), 16#C4#), (Diaeresis, L ('E'), 16#CB#),
      (Diaeresis, L ('I'), 16#CF#), (Diaeresis, L ('O'), 16#D6#),
      (Diaeresis, L ('U'), 16#DC#),
      (Cedilla, L ('C'), 16#C7#));

   Held     : array (Modifier_Key) of Boolean := (others => False);

   Extended : Boolean := False;
   --  The last byte was the E0 prefix.

   Pending_Accent : Key_Code := 0;
   --  The accent of a dead key that was typed, or 0.

   ----------------
   -- Handle_Key --
   ----------------

   procedure Handle_Key
     (Scan_Code : Rose.Words.Word_8;
      Result    : out Key_Press_Array;
      Last      : out Natural)
   is
      Code    : constant Rose.Words.Word_8 := Scan_Code and 16#7F#;
      Pressed : constant Boolean := Scan_Code < 16#80#;
      Key     : constant Key_Entry :=
                  (if Extended
                   then Extended_Key_Code_Table (Code)
                   else Key_Code_Table (Code));

      Has_AltGr : constant Boolean :=
                    Key.Values (AltGr) /= 0
                    or else Key.Values (Shifted_AltGr) /= 0;
      --  The key has AltGr characters, so AltGr chooses them; otherwise
      --  it is Alt.

      function Modifiers return Modifier_Set;
      --  The modifiers that are held.

      procedure Add (Value : Key_Code);
      --  Give Value with the modifiers.

      function Compose (Value : Key_Code) return Key_Code;
      --  Value composed with the pending accent, or 0 if they don't
      --  compose.

      ---------
      -- Add --
      ---------

      procedure Add (Value : Key_Code) is
      begin
         if Last < Result'Last then
            Last := Last + 1;
            Result (Last) := (Value, Modifiers);
         end if;
      end Add;

      -------------
      -- Compose --
      -------------

      function Compose (Value : Key_Code) return Key_Code is
      begin
         for Item of Compositions loop
            if Item.Accent = Pending_Accent then
               if Value = Item.Letter then
                  return Item.Composed;
               elsif Value = Item.Letter + 32 then
                  return Item.Composed + 32;
               end if;
            end if;
         end loop;
         return 0;
      end Compose;

      ---------------
      -- Modifiers --
      ---------------

      function Modifiers return Modifier_Set is
      begin
         return (if Held (Shift_Key) then Shift else 0)
           + (if Held (Control_Key) then Control else 0)
           + (if Held (Alt_Key)
                or else (Held (AltGr_Key) and then not Has_AltGr)
              then Alt else 0);
      end Modifiers;

   begin
      Last := Result'First - 1;

      if Scan_Code = 16#E0# then
         Extended := True;
         return;
      end if;

      Extended := False;

      if Key.Kind = Modifier_Kind then
         Held (Key.Held) := Pressed;
         return;
      elsif not Pressed or else Key.Kind = No_Key then
         return;
      end if;

      if Key.Kind = Named_Kind then

         --  A dead key followed by a named key gives its accent first.

         if Pending_Accent /= 0 then
            Add (Pending_Accent);
            Pending_Accent := 0;
         end if;

         Add (Key.Named_Key);
         return;
      end if;

      declare
         Chosen : constant Level :=
                    (if Held (AltGr_Key) and then Has_AltGr
                     then (if Held (Shift_Key)
                           then Shifted_AltGr else AltGr)
                     else (if Held (Shift_Key) then Shifted else Base));
         Value  : Key_Code := Key.Values (Chosen);
      begin
         if Value = 0 then
            return;
         end if;

         if Key.Dead (Chosen) then

            --  A second dead key gives the first one's accent.

            if Pending_Accent /= 0 then
               Add (Pending_Accent);
            end if;
            Pending_Accent := Value;
            return;
         end if;

         --  Control with a letter gives the letter's position modulo 32,
         --  so Ctrl-D is EOT (ADR 0011, section 1).

         if Held (Control_Key)
           and then (Value in L ('a') .. L ('z')
                     or else Value in L ('A') .. L ('Z'))
         then
            Value := Value mod 32;
         end if;

         if Pending_Accent /= 0 then
            declare
               Composed : constant Key_Code := Compose (Value);
            begin
               if Composed /= 0 then
                  Add (Composed);
               elsif Value = L (' ') then
                  Add (Pending_Accent);
               else
                  Add (Pending_Accent);
                  Add (Value);
               end if;
               Pending_Accent := 0;
            end;
         else
            Add (Value);
         end if;
      end;
   end Handle_Key;

end Keyboard.Codes;
