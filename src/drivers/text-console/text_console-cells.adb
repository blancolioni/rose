package body Text_Console.Cells is

   type Cell is
      record
         Item      : Character := ' ';
         Attribute : Attributes := Plain;
      end record;

   Blank : constant Cell := (' ', Plain);

   Grid : array (0 .. Max_Rows - 1, 0 .. Max_Columns - 1) of Cell :=
            (others => (others => Blank));

   Num_Columns : Positive := Max_Columns;
   Num_Rows    : Positive := Max_Rows;
   Line        : Natural := 0;
   Column      : Natural := 0;

   Wrap_Pending : Boolean := False;
   --  The last character written was in the last column, and the cursor
   --  is still on it: the next character starts a new line first.

   Current : Attributes := Plain;
   --  The attributes of what is written.

   procedure Draw_Cell (Column, Row : Natural);
   --  Draw the cell from the grid.

   procedure Finish;
   --  Show the cursor, and flush what was drawn.

   procedure New_Line;
   --  Move to the start of the next line, scrolling if it is past the
   --  last row.

   procedure Put_Character (Ch : Character);
   --  Draw Ch, or act on it if it is a control character.

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      Grid := (others => (others => Blank));
      Clear;
      Line := 0;
      Column := 0;
      Wrap_Pending := False;
      Finish;
   end Clear_Screen;

   --------------------------
   -- Clear_To_End_Of_Line --
   --------------------------

   procedure Clear_To_End_Of_Line is
   begin
      for C in Column .. Num_Columns - 1 loop
         Grid (Line, C) := Blank;
         Draw_Cell (C, Line);
      end loop;
      Finish;
   end Clear_To_End_Of_Line;

   -------------
   -- Columns --
   -------------

   function Columns return Positive is (Num_Columns);

   ---------------
   -- Draw_Cell --
   ---------------

   procedure Draw_Cell (Column, Row : Natural) is
   begin
      Draw (Column, Row, Grid (Row, Column).Item,
            Grid (Row, Column).Attribute);
   end Draw_Cell;

   ------------
   -- Finish --
   ------------

   procedure Finish is
   begin
      Show_Cursor (Column, Line);
      Flush;
   end Finish;

   -----------
   -- Go_To --
   -----------

   procedure Go_To (Column, Row : Natural) is
   begin
      Move_Cursor (Column, Row);
      Finish;
   end Go_To;

   ----------
   -- Load --
   ----------

   procedure Load
     (Column, Row : Natural;
      Item        : Character)
   is
   begin
      if Column < Num_Columns and then Row < Num_Rows then
         Grid (Row, Column) := (Item, Plain);
      end if;
   end Load;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor (Column, Row : Natural) is
   begin
      Cells.Column := Natural'Min (Column, Num_Columns - 1);
      Line := Natural'Min (Row, Num_Rows - 1);
      Wrap_Pending := False;
   end Move_Cursor;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Line := Line + 1;
      Column := 0;
      Wrap_Pending := False;

      if Line >= Num_Rows then
         declare
            Lines : constant Positive := Line - Num_Rows + 1;
         begin
            for Row in 0 .. Num_Rows - 1 loop
               for C in 0 .. Num_Columns - 1 loop
                  Grid (Row, C) :=
                    (if Row + Lines < Num_Rows
                     then Grid (Row + Lines, C)
                     else Blank);
               end loop;
            end loop;
            Line := Num_Rows - 1;
            Scroll (Lines);
         end;
      end if;
   end New_Line;

   -------------
   -- Operate --
   -------------

   procedure Operate (Operation : Screen_Operation) is
   begin
      case Operation.Kind is
         when Move_Cursor =>
            Go_To (Operation.Column, Operation.Row);
         when Clear =>
            Clear_Screen;
         when Clear_To_End_Of_Line =>
            Clear_To_End_Of_Line;
         when Scroll =>
            Scroll_Lines (Operation.Lines);
         when Set_Attributes =>
            Set_Attributes (Operation.Attribute);
      end case;
   end Operate;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      for Ch of Text loop
         Put_Character (Ch);
      end loop;

      Finish;
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
         --  line that wrapped can be erased (#397). After a character in
         --  the last column, the cursor is still on it.

         if Wrap_Pending then
            Wrap_Pending := False;
         elsif Column > 0 then
            Column := Column - 1;
         elsif Line > 0 then
            Line := Line - 1;
            Column := Num_Columns - 1;
         end if;
      elsif Character'Pos (Ch) = 9 then
         if Wrap_Pending then
            New_Line;
         end if;
         Column := (Column / 8 + 1) * 8;
         if Column >= Num_Columns then
            New_Line;
         end if;
      elsif Character'Pos (Ch) = 13 then
         Column := 0;
         Wrap_Pending := False;
      elsif Character'Pos (Ch) < 32 or else Character'Pos (Ch) = 127 then

         --  Other control characters, including escape, draw nothing.

         null;
      else
         if Wrap_Pending then
            New_Line;
         end if;

         Grid (Line, Column) := (Ch, Current);
         Draw_Cell (Column, Line);

         if Column = Num_Columns - 1 then
            Wrap_Pending := True;
         else
            Column := Column + 1;
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
            Draw_Cell (C, Row);
         end loop;
      end loop;
      Finish;
   end Redraw;

   ----------
   -- Rows --
   ----------

   function Rows return Positive is (Num_Rows);

   ------------------
   -- Scroll_Lines --
   ------------------

   procedure Scroll_Lines (Lines : Integer) is
   begin
      if Lines > 0 then
         declare
            Count : constant Positive := Natural'Min (Lines, Num_Rows);
         begin
            for Row in 0 .. Num_Rows - 1 loop
               for C in 0 .. Num_Columns - 1 loop
                  Grid (Row, C) :=
                    (if Row + Count < Num_Rows
                     then Grid (Row + Count, C)
                     else Blank);
               end loop;
            end loop;
            Scroll (Count);
         end;
         Finish;
      elsif Lines < 0 then

         --  The drawing can only scroll up, so scrolling down draws every
         --  cell again.

         declare
            Count : constant Positive :=
                      (if Lines < -Num_Rows then Num_Rows else -Lines);
         begin
            for Row in reverse 0 .. Num_Rows - 1 loop
               for C in 0 .. Num_Columns - 1 loop
                  Grid (Row, C) :=
                    (if Row >= Count
                     then Grid (Row - Count, C)
                     else Blank);
               end loop;
            end loop;
         end;
         Redraw;
      end if;
   end Scroll_Lines;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes (Attribute : Attributes) is
   begin
      Current := Attribute;
   end Set_Attributes;

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
      Grid := (others => (others => Blank));
      Line := 0;
      Column := 0;
      Wrap_Pending := False;
   end Set_Size;

end Text_Console.Cells;
