package body Trellis_Layout is

   use Rose.Words;

   ----------
   -- Clip --
   ----------

   function Clip
     (X, Y          : Rose.Words.Word;
      Width, Height : Rose.Words.Word;
      Limit_Width   : Natural;
      Limit_Height  : Natural)
      return Rectangle
   is
      function Cut
        (Start, Length : Word;
         Limit         : Natural)
         return Natural;
      --  How much of Start .. Start + Length - 1 is below Limit

      ---------
      -- Cut --
      ---------

      function Cut
        (Start, Length : Word;
         Limit         : Natural)
         return Natural
      is
      begin
         if Word_64 (Start) >= Word_64 (Limit) then
            return 0;
         else
            return Natural
              (Word_64'Min (Word_64 (Length),
                            Word_64 (Limit) - Word_64 (Start)));
         end if;
      end Cut;

      Cut_Width  : constant Natural := Cut (X, Width, Limit_Width);
      Cut_Height : constant Natural := Cut (Y, Height, Limit_Height);
   begin
      if Cut_Width = 0 or else Cut_Height = 0 then
         return (others => 0);
      else
         return (Natural (X), Natural (Y), Cut_Width, Cut_Height);
      end if;
   end Clip;

   --------------
   -- Is_Weave --
   --------------

   function Is_Weave (X, Y : Natural) return Boolean is
      Weave : constant array (0 .. 3) of Word_8 :=
                (16#07#, 16#0D#, 16#0B#, 16#0E#);
      --  root_weave.xbm: a row each, the lowest bit leftmost, a set bit
      --  black.
   begin
      return (Weave (Y mod 4) / 2 ** (X mod 4)) mod 2 = 1;
   end Is_Weave;

   ----------
   -- Tile --
   ----------

   function Tile
     (Count  : Positive;
      Index  : Positive;
      Width  : Natural;
      Height : Natural)
      return Rectangle
   is
   begin
      if Count = 2 then
         declare
            Left : constant Natural := Width / 2;
         begin
            return (if Index = 1
                    then (0, 0, Left, Height)
                    else (Left, 0, Width - Left, Height));
         end;
      else
         return (0, 0, Width, Height);
      end if;
   end Tile;

end Trellis_Layout;
