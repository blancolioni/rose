with Ada.Command_Line;
with Ada.Text_IO;

with Rose.Words;                 use Rose.Words;

with Trellis_Layout;             use Trellis_Layout;

procedure Test_Trellis_Layout is

   --  The tests of trellis's layout arithmetic (#583), run on the host.
   --  Each check is reported, and the exit status is 1 if any failed.

   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Name : String; Ok : Boolean);

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

   Last : constant Word := Word'Last;

begin

   --  Clipping a client's damage to its surface.

   Check ("inside is unchanged",
          Clip (10, 20, 30, 40, 320, 200) = (10, 20, 30, 40));
   Check ("cut at the right and the bottom",
          Clip (300, 190, 50, 50, 320, 200) = (300, 190, 20, 10));
   Check ("all of it", Clip (0, 0, 320, 200, 320, 200) = (0, 0, 320, 200));
   Check ("past the right edge is empty",
          Is_Empty (Clip (320, 0, 10, 10, 320, 200)));
   Check ("past the bottom edge is empty",
          Is_Empty (Clip (0, 200, 10, 10, 320, 200)));
   Check ("no width is empty", Is_Empty (Clip (5, 5, 0, 10, 320, 200)));
   Check ("the largest words don't overflow",
          Clip (300, 0, Last, Last, 320, 200) = (300, 0, 20, 200));
   Check ("a start past Natural'Last is empty",
          Is_Empty (Clip (Last, Last, Last, Last, 320, 200)));
   Check ("a surface of no size takes nothing",
          Is_Empty (Clip (0, 0, 10, 10, 0, 0)));

   --  Moving from a window's pixels to the root's.

   Check ("offset", Offset ((1, 2, 3, 4), 160, 0) = (161, 2, 3, 4));

   --  Tiles.

   Check ("one window has the root",
          Tile (1, 1, 320, 200) = (0, 0, 320, 200));
   Check ("two windows: the left half",
          Tile (2, 1, 320, 200) = (0, 0, 160, 200));
   Check ("two windows: the right half",
          Tile (2, 2, 320, 200) = (160, 0, 160, 200));
   Check ("an odd width: the odd pixel is on the right",
          Tile (2, 1, 321, 200) = (0, 0, 160, 200)
          and then Tile (2, 2, 321, 200) = (160, 0, 161, 200));
   Check ("the halves cover the root, and don't overlap",
          Tile (2, 1, 1023, 768).Width + Tile (2, 2, 1023, 768).Width = 1023
          and then Tile (2, 2, 1023, 768).X = Tile (2, 1, 1023, 768).Width);

   --  The weave.

   declare
      Count : Natural := 0;
   begin
      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            if Is_Weave (X, Y) then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      Check ("the weave is three quarters black", Count = 12);
   end;
   Check ("the weave's first row: black, black, black, white",
          Is_Weave (0, 0) and then Is_Weave (1, 0) and then Is_Weave (2, 0)
          and then not Is_Weave (3, 0));
   Check ("the weave repeats every four pixels",
          (for all Y in 0 .. 7 =>
             (for all X in 0 .. 7 =>
                Is_Weave (X, Y) = Is_Weave (X + 4, Y + 4))));

   Ada.Text_IO.Put_Line
     (Checks'Image & " checks," & Failures'Image & " failed");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Trellis_Layout;
