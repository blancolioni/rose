with System.Storage_Elements;

package body Rose.Drawing is

   use System.Storage_Elements;

   type Pixel_Row is array (Natural range <>) of Pixel;

   function Row_Address
     (Target : Bitmap;
      Y      : Natural)
      return System.Address
   is (Target.Pixels
       + Storage_Offset (Y) * Storage_Offset (Target.Stride) * 4);
   --  The address of the first pixel of row Y of Target

   procedure Clip
     (Target : Bitmap;
      Area   : in out Rectangle);
   --  Cut Area to Target.

   procedure Clip_Copy
     (Source      : Bitmap;
      Area        : in out Rectangle;
      Target      : Bitmap;
      X, Y        : in out Integer);
   --  Cut Area to Source, and the copy of it at X, Y to Target, moving X,
   --  Y and Area together so that each pixel still goes where it did.

   ----------
   -- Clip --
   ----------

   procedure Clip
     (Target : Bitmap;
      Area   : in out Rectangle)
   is
      Left   : constant Integer := Integer'Max (Area.X, 0);
      Top    : constant Integer := Integer'Max (Area.Y, 0);
      Right  : constant Integer :=
                 Integer'Min (Area.X + Area.Width, Target.Width);
      Bottom : constant Integer :=
                 Integer'Min (Area.Y + Area.Height, Target.Height);
   begin
      if Right <= Left or else Bottom <= Top then
         Area := (0, 0, 0, 0);
      else
         Area := (Left, Top, Right - Left, Bottom - Top);
      end if;
   end Clip;

   ---------------
   -- Clip_Copy --
   ---------------

   procedure Clip_Copy
     (Source      : Bitmap;
      Area        : in out Rectangle;
      Target      : Bitmap;
      X, Y        : in out Integer)
   is
      Before  : constant Rectangle := Area;
      Landing : Rectangle;
   begin
      Clip (Source, Area);
      X := X + (Area.X - Before.X);
      Y := Y + (Area.Y - Before.Y);

      Landing := (X, Y, Area.Width, Area.Height);
      Clip (Target, Landing);
      if Landing.Width = 0 then
         Area := (0, 0, 0, 0);
         return;
      end if;

      Area := (Area.X + (Landing.X - X), Area.Y + (Landing.Y - Y),
               Landing.Width, Landing.Height);
      X := Landing.X;
      Y := Landing.Y;
   end Clip_Copy;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Source : Bitmap;
      Area   : Rectangle;
      Target : Bitmap;
      X, Y   : Integer)
   is
      From         : Rectangle := Area;
      To_X         : Integer := X;
      To_Y         : Integer := Y;
   begin
      Clip_Copy (Source, From, Target, To_X, To_Y);
      for Row in 0 .. From.Height - 1 loop
         declare
            Source_Row : Pixel_Row (0 .. Source.Width - 1)
              with Import, Address => Row_Address (Source, From.Y + Row);
            Target_Row : Pixel_Row (0 .. Target.Width - 1)
              with Import, Address => Row_Address (Target, To_Y + Row);
         begin
            Target_Row (To_X .. To_X + From.Width - 1) :=
              Source_Row (From.X .. From.X + From.Width - 1);
         end;
      end loop;
   end Copy;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Target : Bitmap;
      Area   : Rectangle;
      Colour : Pixel)
   is
      Inside : Rectangle := Area;
   begin
      Clip (Target, Inside);
      for Row in Inside.Y .. Inside.Y + Inside.Height - 1 loop
         declare
            Line : Pixel_Row (0 .. Target.Width - 1)
              with Import, Address => Row_Address (Target, Row);
         begin
            Line (Inside.X .. Inside.X + Inside.Width - 1) :=
              (others => Colour);
         end;
      end loop;
   end Fill;

   ---------
   -- Get --
   ---------

   function Get
     (Target : Bitmap;
      X, Y   : Natural)
      return Pixel
   is
      Line : Pixel_Row (0 .. Target.Width - 1)
        with Import, Address => Row_Address (Target, Y);
   begin
      return Line (X);
   end Get;

   ------------
   -- Invert --
   ------------

   procedure Invert
     (Target : Bitmap;
      Area   : Rectangle)
   is
      Inside : Rectangle := Area;
   begin
      Clip (Target, Inside);
      for Row in Inside.Y .. Inside.Y + Inside.Height - 1 loop
         declare
            Line : Pixel_Row (0 .. Target.Width - 1)
              with Import, Address => Row_Address (Target, Row);
         begin
            for Item of Line (Inside.X .. Inside.X + Inside.Width - 1) loop
               Item := Item xor 16#00FF_FFFF#;
            end loop;
         end;
      end loop;
   end Invert;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : Bitmap;
      Area   : Rectangle;
      X, Y   : Integer)
   is
      From : Rectangle := Area;
      To_X : Integer := X;
      To_Y : Integer := Y;

      procedure Move_Row (Row : Natural);
      --  Move row Row of From, with a slice assignment, which is right
      --  when the source and target overlap in the same row.

      --------------
      -- Move_Row --
      --------------

      procedure Move_Row (Row : Natural) is
      begin
         if From.Y + Row = To_Y + Row then
            declare
               Line : Pixel_Row (0 .. Target.Width - 1)
                 with Import, Address => Row_Address (Target, To_Y + Row);
            begin
               Line (To_X .. To_X + From.Width - 1) :=
                 Line (From.X .. From.X + From.Width - 1);
            end;
         else
            declare
               Source_Row : Pixel_Row (0 .. Target.Width - 1)
                 with Import, Address => Row_Address (Target, From.Y + Row);
               Target_Row : Pixel_Row (0 .. Target.Width - 1)
                 with Import, Address => Row_Address (Target, To_Y + Row);
            begin
               Target_Row (To_X .. To_X + From.Width - 1) :=
                 Source_Row (From.X .. From.X + From.Width - 1);
            end;
         end if;
      end Move_Row;

   begin
      Clip_Copy (Target, From, Target, To_X, To_Y);

      --  Rows are moved away from the direction of the move, so that no
      --  row is overwritten before it has been moved.

      if To_Y > From.Y then
         for Row in reverse 0 .. From.Height - 1 loop
            Move_Row (Row);
         end loop;
      else
         for Row in 0 .. From.Height - 1 loop
            Move_Row (Row);
         end loop;
      end if;
   end Move;

   ---------
   -- Set --
   ---------

   procedure Set
     (Target : Bitmap;
      X, Y   : Integer;
      Colour : Pixel)
   is
   begin
      if X in 0 .. Target.Width - 1 and then Y in 0 .. Target.Height - 1 then
         declare
            Line : Pixel_Row (0 .. Target.Width - 1)
              with Import, Address => Row_Address (Target, Y);
         begin
            Line (X) := Colour;
         end;
      end if;
   end Set;

end Rose.Drawing;
