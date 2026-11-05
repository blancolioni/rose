with Ada.Command_Line;
with Ada.Text_IO;

with Rose.Drawing;               use Rose.Drawing;
with Rose.Drawing.Text;

procedure Test_Drawing is

   --  The tests of Rose.Drawing and Rose.Drawing.Text (#579, #580), run on
   --  the host. Each check is reported, and the exit status is 1 if any
   --  failed.

   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Name : String; Ok : Boolean);

   type Buffer is array (Natural range <>) of aliased Pixel;

   function Count
     (Target : Bitmap;
      Colour : Pixel)
      return Natural;
   --  The pixels of Target that are Colour

   -----------
   -- Check --
   -----------

   procedure Check (Name : String; Ok : Boolean) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
      end if;
      Ada.Text_IO.Put_Line ((if Ok then "ok:   " else "FAIL: ") & Name);
   end Check;

   -----------
   -- Count --
   -----------

   function Count
     (Target : Bitmap;
      Colour : Pixel)
      return Natural
   is
      Result : Natural := 0;
   begin
      for Y in 0 .. Target.Height - 1 loop
         for X in 0 .. Target.Width - 1 loop
            if Get (Target, X, Y) = Colour then
               Result := Result + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Count;

begin

   --  Fill, cut to the bitmap

   declare
      Pixels : Buffer (0 .. 59) := (others => 0);
      Target : constant Bitmap := To_Bitmap (Pixels'Address, 10, 6);
   begin
      Fill (Target, (-2, -1, 5, 3), 7);
      Check ("fill is cut at the top left",
             Count (Target, 7) = 6
             and then Get (Target, 0, 0) = 7
             and then Get (Target, 2, 1) = 7
             and then Get (Target, 3, 0) = 0
             and then Get (Target, 0, 2) = 0);

      Fill (Target, (8, 4, 10, 10), 9);
      Check ("fill is cut at the bottom right",
             Count (Target, 9) = 4 and then Get (Target, 9, 5) = 9);

      Fill (Target, (20, 20, 5, 5), 3);
      Fill (Target, (-9, 0, 5, 5), 3);
      Check ("fill outside the bitmap draws nothing",
             Count (Target, 3) = 0);
   end;

   --  Rows with padding between them

   declare
      Pixels : Buffer (0 .. 23) := (others => 0);
      Target : constant Bitmap := (Pixels'Address, 4, 4, 6);
   begin
      Fill (Target, (0, 0, 10, 10), 5);
      Check ("fill leaves the padding alone",
             (for all I in Pixels'Range =>
                Pixels (I) = (if I mod 6 < 4 then 5 else 0)));
   end;

   --  Copy, cut to both bitmaps

   declare
      Source_Pixels : Buffer (0 .. 15);
      Target_Pixels : Buffer (0 .. 35) := (others => 0);
      Source        : constant Bitmap :=
                        To_Bitmap (Source_Pixels'Address, 4, 4);
      Target        : constant Bitmap :=
                        To_Bitmap (Target_Pixels'Address, 6, 6);
   begin
      for I in Source_Pixels'Range loop
         Source_Pixels (I) := Pixel (100 + I);
      end loop;

      Copy (Source, (0, 0, 4, 4), Target, 4, 3);
      Check ("copy is cut at the target's edge",
             Get (Target, 4, 3) = 100
             and then Get (Target, 5, 5) = 100 + 2 * 4 + 1
             and then Count (Target, 0) = 36 - 6);

      Target_Pixels := (others => 0);
      Copy (Source, (0, 0, 4, 4), Target, -1, -1);
      Check ("copy to a negative position takes the right pixels",
             Get (Target, 0, 0) = 100 + 1 * 4 + 1
             and then Get (Target, 2, 2) = 100 + 3 * 4 + 3
             and then Count (Target, 0) = 36 - 9);

      Target_Pixels := (others => 0);
      Copy (Source, (2, 2, 5, 5), Target, 0, 0);
      Check ("copy is cut at the source's edge",
             Get (Target, 0, 0) = 100 + 2 * 4 + 2
             and then Get (Target, 1, 1) = 100 + 3 * 4 + 3
             and then Count (Target, 0) = 36 - 4);
   end;

   --  Move within one bitmap, overlapping

   declare
      Pixels : Buffer (0 .. 14);
      Target : constant Bitmap := To_Bitmap (Pixels'Address, 3, 5);
   begin
      for I in Pixels'Range loop
         Pixels (I) := Pixel (I / 3);
      end loop;
      Move (Target, (0, 0, 3, 4), 0, 1);
      Check ("move down over itself",
             (for all I in Pixels'Range =>
                Pixels (I) = Pixel (Integer'Max (I / 3 - 1, 0))));

      for I in Pixels'Range loop
         Pixels (I) := Pixel (I / 3);
      end loop;
      Move (Target, (0, 1, 3, 4), 0, 0);
      Check ("move up over itself",
             (for all I in 0 .. 11 => Pixels (I) = Pixel (I / 3 + 1))
             and then (for all I in 12 .. 14 => Pixels (I) = 4));
   end;

   declare
      Pixels : Buffer (0 .. 4) := (0, 1, 2, 3, 4);
      Target : constant Bitmap := To_Bitmap (Pixels'Address, 5, 1);
   begin
      Move (Target, (0, 0, 4, 1), 1, 0);
      Check ("move right within a row",
             Pixels = (0, 0, 1, 2, 3));
      Move (Target, (1, 0, 4, 1), 0, 0);
      Check ("move left within a row",
             Pixels = (0, 1, 2, 3, 3));
   end;

   --  Invert

   declare
      Pixels : Buffer (0 .. 3) := (16#FF00_0000#, 16#0012_3456#, 0, 0);
      Target : constant Bitmap := To_Bitmap (Pixels'Address, 2, 2);
   begin
      Invert (Target, (0, 0, 2, 1));
      Check ("invert leaves the top byte",
             Pixels (0) = 16#FFFF_FFFF#
             and then Pixels (1) = 16#00ED_CBA9#
             and then Pixels (2) = 0);
      Invert (Target, (0, 0, 2, 1));
      Check ("inverting twice gives the pixels back",
             Pixels (0) = 16#FF00_0000# and then Pixels (1) = 16#0012_3456#);
   end;

   Check ("RGB puts red in the third byte",
          RGB (16#12#, 16#34#, 16#56#) = 16#0012_3456#);

   --  Text

   declare
      use Rose.Drawing.Text;
      Pixels : Buffer (0 .. 8 * 32 - 1) := (others => 0);
      Target : constant Bitmap := To_Bitmap (Pixels'Address, 8, 32);

      function Row_Bits (Row : Natural) return Natural;
      --  The pixels of Row that are 1, as bits, the leftmost highest

      function Row_Bits (Row : Natural) return Natural is
         Result : Natural := 0;
      begin
         for X in 0 .. 7 loop
            Result := Result * 2 + (if Get (Target, X, Row) = 1 then 1 else 0);
         end loop;
         return Result;
      end Row_Bits;

   begin
      Check ("a 1024 by 768 screen is 128 by 48 cells",
             Columns (To_Bitmap (Pixels'Address, 1024, 768)) = 128
             and then Rows (To_Bitmap (Pixels'Address, 1024, 768)) = 48);

      Draw_Character (Target, 0, 1, 'A', 1, 2);
      Check ("A is drawn from the font",
             Row_Bits (16 + 2) = 16#7C#
             and then Row_Bits (16 + 6) = 16#FE#
             and then Row_Bits (16 + 13) = 0);
      Check ("A's background is filled, and the cell above is not touched",
             Count (Target, 2) + Count (Target, 1) = 128
             and then Count (Target, 0) = 128);

      Draw_Character (Target, 0, 0, Character'Val (7), 1, 2);
      Check ("a control character is a blank cell",
             (for all Y in 0 .. 15 => Row_Bits (Y) = 0)
             and then Get (Target, 3, 3) = 2);

      Pixels := (others => 0);
      Draw_Character (Target, 0, 1, 'A', 1, 2);
      Scroll_Up (Target, 0, 0, 1, 2, 1, 9);
      Check ("scrolling up moves the text and clears the bottom row",
             Row_Bits (2) = 16#7C# and then Row_Bits (6) = 16#FE#
             and then (for all Y in 16 .. 31 =>
                         (for all X in 0 .. 7 => Get (Target, X, Y) = 9)));

      Scroll_Up (Target, 0, 0, 1, 2, 5, 4);
      Check ("scrolling by more rows than there are clears them all",
             Count (Target, 4) = 256);

      Pixels := (others => 0);
      Toggle_Cursor (Target, 0, 1);
      Check ("the cursor inverts the cell's bottom two rows",
             Count (Target, 16#00FF_FFFF#) = 16
             and then Get (Target, 0, 30) = 16#00FF_FFFF#
             and then Get (Target, 7, 31) = 16#00FF_FFFF#
             and then Get (Target, 0, 29) = 0);
      Toggle_Cursor (Target, 0, 1);
      Check ("toggling the cursor again takes it away",
             Count (Target, 0) = 256);

      Draw_Character (To_Bitmap (Pixels'Address, 4, 8), 0, 0, 'A', 1, 2);
      Check ("a cell partly outside the bitmap is cut", True);
   end;

   Ada.Text_IO.Put_Line
     ("drawing tests:" & Natural'Image (Checks) & " checks,"
      & Natural'Image (Failures) & " failed");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Drawing;
