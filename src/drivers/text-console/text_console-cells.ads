generic
   Max_Columns : Positive;
   Max_Rows    : Positive;

   with procedure Draw
     (Column, Row : Natural;
      Item        : Character;
      Attribute   : Attributes);
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
   --  is on it, how each cell is drawn, and where the cursor is. The
   --  generic's procedures draw it, on the VGA text page or a surface. It
   --  is all in the process's memory, so a checkpoint holds the screen,
   --  and Redraw puts it back.
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
   --  Write Text at the cursor, with the attributes. A line feed starts a
   --  new line, backspace moves the cursor back, also to the end of the
   --  line above, tab moves it to the next multiple of 8, and carriage
   --  return to the start of the line. Other control characters,
   --  including escape, draw nothing (#734). A character written in the
   --  last column leaves the cursor there, and the next one starts a new
   --  line, so the bottom right cell can be written without scrolling.
   --  Text that goes past the last row scrolls the screen up.

   --  The screen operations of Terminal (#734). Each draws what it
   --  changes, and the cursor.

   procedure Go_To (Column, Row : Natural);
   --  Move the cursor, to the nearest cell if it is off the screen.

   procedure Clear_Screen;
   --  Blank every cell, and move the cursor to the top left.

   procedure Clear_To_End_Of_Line;
   --  Blank the cells from the cursor to the end of its row.

   procedure Scroll_Lines (Lines : Integer);
   --  Move the rows up by Lines, or down if it is negative, and blank the
   --  rows left behind. The cursor doesn't move.

   procedure Set_Attributes (Attribute : Attributes);
   --  How Put draws from now on.

   procedure Operate (Operation : Screen_Operation);
   --  Do Operation, with the procedure above that does it.

   procedure Redraw;
   --  Draw every cell and the cursor.

end Text_Console.Cells;
