with Rose.Words;

package Trellis_Layout with Pure is

   --  The arithmetic of trellis's layout (#583): where each window goes,
   --  how a client's damage is cut to its surface, and the background.
   --  It does no I/O, so that it is tested on the host ("make
   --  test-surfaces").

   type Rectangle is
      record
         X, Y          : Natural := 0;
         Width, Height : Natural := 0;
      end record;
   --  Pixels from X to X + Width - 1, and from Y to Y + Height - 1.

   function Is_Empty (Area : Rectangle) return Boolean
   is (Area.Width = 0 or else Area.Height = 0);

   function Clip
     (X, Y          : Rose.Words.Word;
      Width, Height : Rose.Words.Word;
      Limit_Width   : Natural;
      Limit_Height  : Natural)
      return Rectangle;
   --  The rectangle X, Y, Width, Height, as a client reports damage, cut
   --  to 0 .. Limit_Width - 1 by 0 .. Limit_Height - 1. Empty if none of
   --  it is inside. No value of the words can overflow.

   function Offset
     (Area : Rectangle;
      X, Y : Natural)
      return Rectangle
   is ((Area.X + X, Area.Y + Y, Area.Width, Area.Height));
   --  Area moved right by X and down by Y, as from a surface's own
   --  pixels to the root's.

   Max_Windows : constant := 8;

   subtype Window_Count is Natural range 0 .. Max_Windows;

   function Tile
     (Count  : Positive;
      Index  : Positive;
      Width  : Natural;
      Height : Natural)
      return Rectangle
     with Pre => Index <= Count and then Count <= Max_Windows;
   --  Where window Index of Count goes on a root of Width by Height: one
   --  window has the whole root, and two share it, side by side, the
   --  first on the left, with the odd pixel on the right. More windows
   --  aren't laid out until #585, and each of them gets the whole root.

   function Is_Weave (X, Y : Natural) return Boolean;
   --  True where the background is black: X's root weave, a 4 by 4
   --  pattern, repeated over the root.

end Trellis_Layout;
