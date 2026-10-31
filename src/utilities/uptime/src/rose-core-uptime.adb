with Ada.Text_IO;

with Rose.Clock.Calendar;

procedure Rose.Core.Uptime
  (Clock_Service : in out Rose.Interfaces.Clock.Clock_Object'Class;
   Seconds       : Boolean := False)
is
   use Rose.Clock.Calendar;

   function Image
     (Value : Natural;
      Width : Positive := 1)
      return String;
   --  Value in at least Width characters, padded on the left with zeros.

   function Plural
     (Count : Natural;
      Unit  : String)
      return String;
   --  Count and Unit, with an s on the unit unless Count is 1.

   -----------
   -- Image --
   -----------

   function Image
     (Value : Natural;
      Width : Positive := 1)
      return String
   is
      Digits_Needed : Positive := 1;
      Rest          : Natural := Value / 10;
   begin
      while Rest > 0 loop
         Digits_Needed := Digits_Needed + 1;
         Rest := Rest / 10;
      end loop;

      declare
         Result : String (1 .. Positive'Max (Width, Digits_Needed)) :=
                    (others => '0');
      begin
         Rest := Value;
         for I in reverse Result'Range loop
            Result (I) := Character'Val (Character'Pos ('0') + Rest mod 10);
            Rest := Rest / 10;
            exit when Rest = 0;
         end loop;
         return Result;
      end;
   end Image;

   ------------
   -- Plural --
   ------------

   function Plural
     (Count : Natural;
      Unit  : String)
      return String
   is (Image (Count) & ' ' & Unit & (if Count = 1 then "" else "s"));

begin
   Rose.Clock.Open (Clock_Service.Clock_Cap);

   declare
      Elapsed : constant Elapsed_Time :=
                  To_Elapsed (Rose.Clock.Monotonic);
   begin
      if Seconds then

         --  The days of a machine that has run for 68 years fit a Natural.

         Ada.Text_IO.Put_Line
           (Image (Elapsed.Days * 86_400
                   + Elapsed.Hours * 3_600
                   + Elapsed.Minutes * 60
                   + Elapsed.Seconds));
      else
         Ada.Text_IO.Put_Line
           ("up " & Plural (Elapsed.Days, "day") & ", "
            & Image (Elapsed.Hours) & ':' & Image (Elapsed.Minutes, 2) & ':'
            & Image (Elapsed.Seconds, 2));
      end if;
   end;
end Rose.Core.Uptime;
