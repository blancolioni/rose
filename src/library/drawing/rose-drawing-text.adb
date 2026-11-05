with Rose.Drawing.Text.Font;

package body Rose.Drawing.Text is

   --------------------
   -- Draw_Character --
   --------------------

   procedure Draw_Character
     (Target     : Bitmap;
      Column     : Natural;
      Row        : Natural;
      Item       : Character;
      Foreground : Pixel;
      Background : Pixel)
   is
      use type Font.Glyph_Row;
      Glyph : Font.Glyph renames Font.Glyphs (Item);
      Left  : constant Natural := Column * Cell_Width;
      Top   : constant Natural := Row * Cell_Height;
   begin
      for Y in Glyph'Range loop
         for X in 0 .. Cell_Width - 1 loop
            Set (Target, Left + X, Top + Y,
                 (if (Glyph (Y) and 2 ** (Cell_Width - 1 - X)) /= 0
                  then Foreground else Background));
         end loop;
      end loop;
   end Draw_Character;

   ---------------
   -- Scroll_Up --
   ---------------

   procedure Scroll_Up
     (Target     : Bitmap;
      Column     : Natural;
      Row        : Natural;
      Columns    : Natural;
      Rows       : Natural;
      Lines      : Positive;
      Background : Pixel)
   is
      Kept : constant Natural := (if Lines < Rows then Rows - Lines else 0);
   begin
      if Kept > 0 then
         Move (Target, Cell (Column, Row + Lines, Columns, Kept),
               Column * Cell_Width, Row * Cell_Height);
      end if;
      Fill (Target, Cell (Column, Row + Kept, Columns, Rows - Kept),
            Background);
   end Scroll_Up;

   -------------------
   -- Toggle_Cursor --
   -------------------

   procedure Toggle_Cursor
     (Target : Bitmap;
      Column : Natural;
      Row    : Natural)
   is
   begin
      Invert (Target,
              (Column * Cell_Width, (Row + 1) * Cell_Height - 2,
               Cell_Width, 2));
   end Toggle_Cursor;

end Rose.Drawing.Text;
