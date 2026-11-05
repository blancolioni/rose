package body Text_Console.Cells is

   Grid : array (0 .. Max_Rows - 1, 0 .. Max_Columns - 1) of Character :=
            (others => (others => ' '));

   Num_Columns : Positive := Max_Columns;
   Num_Rows    : Positive := Max_Rows;
   Line        : Natural := 0;
   Column      : Natural := 0;

   type Escape_State is (Ground, Escape, Sequence);

   Escape_Output : Escape_State := Ground;
   Parameter     : Natural := 0;
   --  Where we are in an escape sequence that is being written, and its
   --  parameter so far (#454).

   procedure End_Sequence (Final : Character);
   --  Act on the escape sequence ESC [ Parameter Final.

   procedure New_Line;
   --  Move to the start of the next line, scrolling if it is past the
   --  last row.

   procedure Put_Character (Ch : Character);
   --  Draw Ch, or act on it if it is a control character.

   -------------
   -- Columns --
   -------------

   function Columns return Positive is (Num_Columns);

   ------------------
   -- End_Sequence --
   ------------------

   procedure End_Sequence (Final : Character) is
   begin
      if Final = 'J' and then Parameter = 2 then
         Grid := (others => (others => ' '));
         Clear;
      elsif Final = 'H' then
         Line := 0;
         Column := 0;
      end if;
   end End_Sequence;

   ----------
   -- Load --
   ----------

   procedure Load
     (Column, Row : Natural;
      Item        : Character)
   is
   begin
      if Column < Num_Columns and then Row < Num_Rows then
         Grid (Row, Column) := Item;
      end if;
   end Load;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor (Column, Row : Natural) is
   begin
      Cells.Column := Natural'Min (Column, Num_Columns - 1);
      Line := Natural'Min (Row, Num_Rows - 1);
   end Move_Cursor;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Line := Line + 1;
      Column := 0;

      if Line >= Num_Rows then
         declare
            Lines : constant Positive := Line - Num_Rows + 1;
         begin
            for Row in 0 .. Num_Rows - 1 loop
               for C in 0 .. Num_Columns - 1 loop
                  Grid (Row, C) :=
                    (if Row + Lines < Num_Rows
                     then Grid (Row + Lines, C)
                     else ' ');
               end loop;
            end loop;
            Line := Num_Rows - 1;
            Scroll (Lines);
         end;
      end if;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      for Ch of Text loop
         case Escape_Output is
            when Ground =>
               Put_Character (Ch);

            when Escape =>
               Escape_Output := (if Ch = '[' then Sequence else Ground);
               Parameter := 0;

            when Sequence =>
               if Ch in '0' .. '9' then
                  Parameter :=
                    Natural'Min (Parameter * 10, 1000)
                    + Character'Pos (Ch) - 48;
               elsif Character'Pos (Ch) in 16#40# .. 16#7E# then
                  End_Sequence (Ch);
                  Escape_Output := Ground;
               end if;
         end case;
      end loop;

      Show_Cursor (Column, Line);
      Flush;
   end Put;

   -------------------
   -- Put_Character --
   -------------------

   procedure Put_Character (Ch : Character) is
   begin
      if Character'Pos (Ch) = 10 then
         New_Line;
      elsif Character'Pos (Ch) = 8 then

         --  Backspace moves the cursor back without erasing, and to the
         --  end of the line above from the start of a line, so that a
         --  line that wrapped can be erased (#397).

         if Column > 0 then
            Column := Column - 1;
         elsif Line > 0 then
            Line := Line - 1;
            Column := Num_Columns - 1;
         end if;
      elsif Character'Pos (Ch) = 9 then
         Column := (Column / 8 + 1) * 8;
         if Column >= Num_Columns then
            New_Line;
         end if;
      elsif Character'Pos (Ch) = 13 then
         Column := 0;
      elsif Character'Pos (Ch) = 27 then
         Escape_Output := Escape;
      elsif Character'Pos (Ch) < 32 or else Character'Pos (Ch) = 127 then

         --  Other control characters draw nothing.

         null;
      else
         Grid (Line, Column) := Ch;
         Draw (Column, Line, Ch);
         Column := Column + 1;
         if Column = Num_Columns then
            New_Line;
         end if;
      end if;
   end Put_Character;

   ------------
   -- Redraw --
   ------------

   procedure Redraw is
   begin
      for Row in 0 .. Num_Rows - 1 loop
         for C in 0 .. Num_Columns - 1 loop
            Draw (C, Row, Grid (Row, C));
         end loop;
      end loop;
      Show_Cursor (Column, Line);
      Flush;
   end Redraw;

   ----------
   -- Rows --
   ----------

   function Rows return Positive is (Num_Rows);

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Columns : Positive;
      Rows    : Positive)
   is
   begin
      Num_Columns := Columns;
      Num_Rows := Rows;
      Grid := (others => (others => ' '));
      Line := 0;
      Column := 0;
   end Set_Size;

end Text_Console.Cells;
