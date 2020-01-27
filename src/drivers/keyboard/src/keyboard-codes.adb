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

   function Asc
     (Base, Shifted, Controled, Alted : Character := Character'Val (0))
      return Key_Entry
   is (Key_Entry'
         (Key_Value => (Base, Shifted, Controled, Alted),
          others    => <>));

   Key_Code : constant array (Rose.Words.Word_8) of Key_Entry :=
                (16#01# => Fn (0),
                 16#0E# => Asc ('`', '~'),
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
