with System;

package Rose.Drawing is

   --  Drawing into a pixel buffer (#579), such as a surface's buffer (ADR
   --  0022). Pixels are 32-bit XRGB. Everything is cut to the bitmap, so
   --  drawing that is partly or wholly outside it is not an error. It
   --  needs nothing from the rest of Rose, and its tests run on the host
   --  (tests/drawing_tests.gpr).

   pragma Preelaborate;

   type Pixel is mod 2 ** 32;
   --  Blue in the low byte, then green, then red. The top byte is unused.

   function RGB (Red, Green, Blue : Natural) return Pixel
   is (Pixel (Red) * 2 ** 16 + Pixel (Green) * 2 ** 8 + Pixel (Blue))
     with Pre => Red < 256 and then Green < 256 and then Blue < 256;

   type Bitmap is
      record
         Pixels : System.Address := System.Null_Address;
         Width  : Natural := 0;
         Height : Natural := 0;
         Stride : Natural := 0;
      end record;
   --  Width by Height pixels at Pixels, row by row from the top left, with
   --  Stride pixels from the start of one row to the start of the next.

   function To_Bitmap
     (Pixels : System.Address;
      Width  : Natural;
      Height : Natural)
      return Bitmap
   is ((Pixels, Width, Height, Width));
   --  A bitmap with no padding between its rows, as a surface's buffer is.

   type Rectangle is
      record
         X, Y   : Integer := 0;
         Width  : Natural := 0;
         Height : Natural := 0;
      end record;

   function Get
     (Target : Bitmap;
      X, Y   : Natural)
      return Pixel
     with Pre => X < Target.Width and then Y < Target.Height;

   procedure Set
     (Target : Bitmap;
      X, Y   : Integer;
      Colour : Pixel);
   --  Set the pixel at X, Y, if it is in Target.

   procedure Fill
     (Target : Bitmap;
      Area   : Rectangle;
      Colour : Pixel);

   procedure Invert
     (Target : Bitmap;
      Area   : Rectangle);
   --  Invert the colour of each pixel in Area, leaving the top byte as it
   --  is. Inverting twice gives the pixels back, as for a cursor.

   procedure Copy
     (Source : Bitmap;
      Area   : Rectangle;
      Target : Bitmap;
      X, Y   : Integer);
   --  Copy Area of Source to Target, with its top left corner at X, Y. It
   --  is cut to both bitmaps. Source and Target must not share pixels: use
   --  Move within one bitmap.

   procedure Move
     (Target : Bitmap;
      Area   : Rectangle;
      X, Y   : Integer);
   --  Copy Area of Target to X, Y in Target, where the two may overlap, as
   --  when scrolling. It is cut to Target.

end Rose.Drawing;
