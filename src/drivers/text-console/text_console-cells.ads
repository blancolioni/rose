generic
   Max_Columns : Positive;
   Max_Rows    : Positive;

   with procedure Draw (Column, Row : Natural; Item : Character);
   --  Draw Item in its cell.

   with procedure Scroll (Lines : Positive);
   --  Move every row up by Lines, and blank the rows left at the bottom.

   with procedure Clear;
   --  Blank every cell.

   with procedure Show_Cursor (Column, Row : Natural);
   --  The cursor is now at Column, Row.

   with procedure Flush;
   --  A Put or a Redraw has finished: show what it drew, if the screen
   --  doesn't show it already.

package Text_Console.Cells is

   --  The console's screen as cells of characters (ADR 0005, #653): what
   --  is on it, where the cursor is, and the escape sequences that it
   --  acts on. The generic's procedures draw it, on the VGA text page or
   --  a surface. It is all in the process's memory, so a checkpoint holds
   --  the screen, and Redraw puts it back.
   --
   --  Columns and rows are counted from 0, from the top left.

   procedure Set_Size
     (Columns : Positive;
      Rows    : Positive)
     with Pre => Columns <= Max_Columns and then Rows <= Max_Rows;
   --  Blank every cell, and put the cursor at the top left. Nothing is
   --  drawn.

   function Columns return Positive;
   function Rows return Positive;

   procedure Load
     (Column, Row : Natural;
      Item        : Character);
   --  Set a cell without drawing it, as when taking over a screen that
   --  already shows something.

   procedure Move_Cursor (Column, Row : Natural);
   --  Move the cursor without drawing it.

   procedure Put (Text : String);
   --  Write Text at the cursor. A line feed starts a new line, backspace
   --  moves the cursor back, also to the end of the line above, tab moves
   --  it to the next multiple of 8, and carriage return to the start of
   --  the line. ESC [ 2 J clears the screen, and ESC [ H moves the cursor
   --  to the top left; other escape sequences and control characters
   --  draw nothing. Text that reaches the last row scrolls the screen up.

   procedure Redraw;
   --  Draw every cell and the cursor.

end Text_Console.Cells;
