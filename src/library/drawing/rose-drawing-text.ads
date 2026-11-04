package Rose.Drawing.Text is

   --  Text in a bitmap, in cells of the 8 by 16 Spleen font (#580): a
   --  1024 by 768 screen is 128 columns by 48 rows. Columns and rows are
   --  counted in cells from the top left, and a cell outside the bitmap,
   --  or partly outside it, is cut to it.

   pragma Preelaborate;

   Cell_Width  : constant := 8;
   Cell_Height : constant := 16;

   function Columns (Target : Bitmap) return Natural
   is (Target.Width / Cell_Width);

   function Rows (Target : Bitmap) return Natural
   is (Target.Height / Cell_Height);

   function Cell
     (Column, Row : Natural;
      Columns     : Natural := 1;
      Rows        : Natural := 1)
      return Rectangle
   is ((Column * Cell_Width, Row * Cell_Height,
        Columns * Cell_Width, Rows * Cell_Height));
   --  The pixels of Columns by Rows cells, from Column, Row

   procedure Draw_Character
     (Target     : Bitmap;
      Column     : Natural;
      Row        : Natural;
      Item       : Character;
      Foreground : Pixel;
      Background : Pixel);
   --  Draw Item in its cell. A character with no glyph, such as a control
   --  character, is drawn as a blank cell.

   procedure Scroll_Up
     (Target     : Bitmap;
      Column     : Natural;
      Row        : Natural;
      Columns    : Natural;
      Rows       : Natural;
      Lines      : Positive;
      Background : Pixel);
   --  Move the text in the Columns by Rows cells from Column, Row up by
   --  Lines rows, and clear the rows at the bottom that it leaves.

   procedure Toggle_Cursor
     (Target : Bitmap;
      Column : Natural;
      Row    : Natural);
   --  Draw an underline cursor in a cell by inverting its bottom two rows
   --  of pixels, or take it away again.

end Rose.Drawing.Text;
