with VGA_Console.Screen;

package body VGA_Console.Line_Discipline is

   use System.Storage_Elements;

   Cooked_Mode  : constant Boolean := True;

   EOT : constant Storage_Element := 4;
   ETX : constant Storage_Element := 3;
   BS  : constant Storage_Element := 8;
   HT  : constant Storage_Element := 9;
   LF  : constant Storage_Element := 10;
   NAK : constant Storage_Element := 21;
   ETB : constant Storage_Element := 23;
   ESC : constant Storage_Element := 27;
   DEL : constant Storage_Element := 127;

   Input_Buffer : Storage_Array (1 .. 4096);
   Input_Last   : Storage_Count := 0;
   Line_Start   : Storage_Count := 0;
   --  Input_Buffer (1 .. Line_Start) are the lines that are complete and
   --  haven't been read, and Input_Buffer (Line_Start + 1 .. Input_Last)
   --  is the line being typed.

   End_Of_File  : Boolean := False;
   --  An end of file that hasn't been read yet (ADR 0011, section 3).

   Echo_Input   : Boolean := True;

   type Escape_State is (Ground, Escape, Sequence);

   Escape_Input : Escape_State := Ground;
   --  Where we are in an escape sequence, which cooked mode drops.

   procedure Append
     (Element  : Storage_Element;
      Complete : in out Boolean);
   --  Add Element to the line being typed, and echo it in cooked mode.
   --  Complete is set if that completes the line.

   procedure Echo (Text : String);

   procedure Rub_Out;
   --  Remove the last character of the line being typed, and erase it
   --  from the screen.

   ------------
   -- Append --
   ------------

   procedure Append
     (Element  : Storage_Element;
      Complete : in out Boolean)
   is
   begin
      if Input_Last < Input_Buffer'Last then
         Input_Last := Input_Last + 1;
         Input_Buffer (Input_Last) := Element;
         if Cooked_Mode then
            Echo ((1 => Character'Val (Element)));
         end if;
      end if;

      if not Cooked_Mode
        or else Element = LF
        or else Input_Last = Input_Buffer'Last
      then
         Line_Start := Input_Last;
         Complete := True;
      end if;
   end Append;

   ----------
   -- Echo --
   ----------

   procedure Echo (Text : String) is
   begin
      if Echo_Input then
         VGA_Console.Screen.Put (Text);
      end if;
   end Echo;

   ------------
   -- On_Key --
   ------------

   procedure On_Key
     (Code  : Rose.Words.Word;
      Ready : out Boolean)
   is
      use type Rose.Words.Word;
      Element : constant Storage_Element :=
                  Storage_Element (Code mod 2 ** System.Storage_Unit);
   begin
      Ready := False;

      if not Cooked_Mode then
         Append (Element, Ready);
         return;
      end if;

      case Escape_Input is
         when Ground =>
            null;
         when Escape =>
            Escape_Input := (if Element = Character'Pos ('[')
                             then Sequence
                             else Ground);
            return;
         when Sequence =>

            --  An escape sequence ends with a byte in 16#40# .. 16#7E#.

            if Element in 16#40# .. 16#7E# then
               Escape_Input := Ground;
            end if;
            return;
      end case;

      case Element is
         when LF | HT | 32 .. 126 =>
            Append (Element, Ready);

         when BS | DEL =>
            Rub_Out;

         when NAK =>
            while Input_Last > Line_Start loop
               Rub_Out;
            end loop;

         when ETB =>
            while Input_Last > Line_Start
              and then Input_Buffer (Input_Last) = Character'Pos (' ')
            loop
               Rub_Out;
            end loop;
            while Input_Last > Line_Start
              and then Input_Buffer (Input_Last) /= Character'Pos (' ')
            loop
               Rub_Out;
            end loop;

         when ETX =>

            --  Discard the line. Once ADR 0006 is implemented, this also
            --  sends Interrupt to the job that is reading.

            Input_Last := Line_Start;
            Echo ("^C" & Character'Val (10));

         when EOT =>

            --  End of file (ADR 0011, section 2). A line that is partly
            --  typed is returned without a line break, and a second
            --  Ctrl-D then ends the input.

            if Input_Last = Line_Start then
               End_Of_File := True;
            end if;
            Line_Start := Input_Last;
            Ready := True;

         when ESC =>
            Escape_Input := Escape;

         when others =>

            --  Other control characters, including Ctrl-Z until ADR 0006
            --  gives it Stop, are ignored.

            null;
      end case;
   end On_Key;

   -----------
   -- Ready --
   -----------

   function Ready return Boolean is
   begin
      return Line_Start > 0 or else End_Of_File;
   end Ready;

   -------------
   -- Rub_Out --
   -------------

   procedure Rub_Out is
   begin
      if Input_Last > Line_Start then
         Input_Last := Input_Last - 1;
         Echo ((Character'Val (BS), ' ', Character'Val (BS)));
      end if;
   end Rub_Out;

   --------------
   -- Set_Echo --
   --------------

   procedure Set_Echo (Enabled : Boolean) is
   begin
      Echo_Input := Enabled;
   end Set_Echo;

   ----------
   -- Take --
   ----------

   procedure Take
     (Buffer : out System.Storage_Elements.Storage_Array;
      Last   : out System.Storage_Elements.Storage_Count)
   is
   begin
      if Line_Start > 0 then
         declare
            Count : constant Storage_Count :=
                      Storage_Count'Min (Buffer'Length, Line_Start);
         begin
            Last := Buffer'First + Count - 1;
            Buffer (Buffer'First .. Last) := Input_Buffer (1 .. Count);
            Input_Buffer (1 .. Input_Last - Count) :=
              Input_Buffer (Count + 1 .. Input_Last);
            Input_Last := Input_Last - Count;
            Line_Start := Line_Start - Count;
         end;
      else

         --  A Read that returns nothing is the end of the stream.

         End_Of_File := False;
         Last := Buffer'First - 1;
      end if;
   end Take;

end VGA_Console.Line_Discipline;
