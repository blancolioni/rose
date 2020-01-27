package body Keyboard.Codes is

   ESC : constant Character := Character'Val (27);

   type Key_Modifier is
     (None, Shift, Control, Alt);

   type Modified_Key_Value is array (Key_Modifier) of Character;

   type Key_Entry is
      record
         Modifier   : Key_Modifier := None;
         Fn_Key     : Natural := 0;
         Key_Value  : Modified_Key_Value := (others => Character'Val (0));
      end record;

   function Fn (N : Natural) return Key_Entry
   is ((Fn_Key => N, others => <>));

   function Modifier (M : Key_Modifier) return Key_Entry
   is (Modifier => M, others => <>);

   function Asc
     (Base, Shifted, Controled, Alted : Character := Character'Val (0))
      return Key_Entry
   is (Key_Entry'
         (Key_Value => (Base, Shifted, Controled, Alted),
          others    => <>));

   Key_Code : constant array (Rose.Words.Word_8) of Key_Entry :=
     (16#2A# => Modifier (Shift),
      16#36# => Modifier (Shift),
      16#1D# => Modifier (Control),
      16#38# => Modifier (Alt),
      16#39# => Asc (' ', ' '),
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
      16#D9# => Fn (11),
      16#DA# => Fn (12),
      16#01# => Asc (ESC, ESC),
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
      16#0E# => Asc (Character'Val (8), Character'Val (8)),
      16#0F# => Asc (Character'Val (9)),
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
      16#1C# => Asc (Character'Val (10)),
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

   Current_Mod : array (Key_Modifier) of Boolean := (others => False);

   ----------------
   -- Handle_Key --
   ----------------

   procedure Handle_Key
     (Code    : Rose.Words.Word;
      Pressed : Boolean;
      Result  : out System.Storage_Elements.Storage_Array;
      Last    : out System.Storage_Elements.Storage_Count)
   is
      use type System.Storage_Elements.Storage_Offset;
      Key : Key_Entry renames Key_Code (Rose.Words.Word_8 (Code));

      procedure Copy (Sequence : String);

      procedure Escape_Sequence (Sequence : String);

      ----------
      -- Copy --
      ----------

      procedure Copy (Sequence : String) is
      begin
         for Ch of Sequence loop
            exit when Last >= Result'Last;
            Last := Last + 1;
            Result (Last) := Character'Pos (Ch);
         end loop;
      end Copy;

      ---------------------
      -- Escape_Sequence --
      ---------------------

      procedure Escape_Sequence (Sequence : String) is
      begin
         Copy ((ESC, '['));
         Copy (Sequence);
         Copy ((1 => '~'));
      end Escape_Sequence;

   begin
      Last := Result'First - 1;

      if not Pressed then
         if Key.Modifier /= None then
            Current_Mod (Key.Modifier) := False;
         end if;
      elsif Key.Modifier /= None then
         Current_Mod (Key.Modifier) := True;
      elsif Key.Fn_Key /= 0 then
         declare
            Modified_Code : constant Natural :=
                              Key.Fn_Key
                                + (if Current_Mod (Shift) then 12 else 0)
                              + (if Current_Mod (Control) then 24 else 0)
                              + (if Current_Mod (Alt) then 48 else 0);
            Sequence      : constant String :=
                              (Character'Val (Modified_Code / 10 + 48),
                               Character'Val (Modified_Code mod 10 + 48));
         begin
            Escape_Sequence (Sequence);
         end;
      elsif Key.Key_Value (None) /= Character'Val (0) then
         declare
            Modifier : Key_Modifier := None;
         begin
            for Pressed_Mod in reverse Key_Modifier loop
               if Current_Mod (Pressed_Mod) then
                  Modifier := Pressed_Mod;
                  exit;
               end if;
            end loop;
            Copy ((1 => Key.Key_Value (Modifier)));
         end;
      end if;
   end Handle_Key;

end Keyboard.Codes;
