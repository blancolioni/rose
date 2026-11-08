with System.Storage_Elements;

with Rose.Drawing.Text.Font;

package body Rose.Drawing.Text is

   type Cell_Row is array (0 .. Cell_Width - 1) of Pixel;
   --  The pixels of one row of a cell

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
      if Left + Cell_Width <= Target.Width
        and then Top + Cell_Height <= Target.Height
      then

         --  The whole cell is in Target, so each of its rows is drawn
         --  straight from the row's address, without cutting each pixel
         --  to Target (#787).

         for Y in Glyph'Range loop
            declare
               use System.Storage_Elements;
               Line : Cell_Row
                 with Import,
                      Address =>
                        Target.Pixels
                        + Storage_Offset
                            ((Top + Y) * Target.Stride + Left) * 4;
               Bits : constant Font.Glyph_Row := Glyph (Y);
            begin
               for X in Line'Range loop
                  Line (X) :=
                    (if (Bits and 2 ** (Cell_Width - 1 - X)) /= 0
                     then Foreground else Background);
               end loop;
            end;
         end loop;

      else
         for Y in Glyph'Range loop
            for X in 0 .. Cell_Width - 1 loop
               Set (Target, Left + X, Top + Y,
                    (if (Glyph (Y) and 2 ** (Cell_Width - 1 - X)) /= 0
                     then Foreground else Background));
            end loop;
         end loop;
      end if;
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
