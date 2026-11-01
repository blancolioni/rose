with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Rose.Capabilities;
with Rose.Clock;
with Rose.Words;

with Tests.Registry;

package body Tests.Ada_Calendar is

   use Rose.Words;
   use type Rose.Capabilities.Capability;
   use type Ada.Calendar.Time_Zones.Time_Offset;

   function To_Word is new Ada.Unchecked_Conversion (Duration, Word_64);
   --  A Duration is a count of nanoseconds (see System.OS_Primitives).

   Tick : constant Word_64 := 10_000_150;
   --  Nanoseconds in a tick.

   -----------------------
   -- Ada_Calendar_Test --
   -----------------------

   function Ada_Calendar_Test return Boolean is
      use Ada.Calendar;

      Passed : Boolean := True;

      procedure Round_Trip
        (Year    : Year_Number;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Day_Duration);
      --  Check that a date survives Time_Of and Split.

      procedure Round_Trip
        (Year    : Year_Number;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Day_Duration)
      is
         Y : Year_Number;
         M : Month_Number;
         D : Day_Number;
         S : Day_Duration;
      begin
         Split (Time_Of (Year, Month, Day, Seconds), Y, M, D, S);
         if Y /= Year or else M /= Month or else D /= Day or else S /= Seconds
         then
            Passed := False;
            Ada.Text_IO.Put (": round trip");
         end if;
      end Round_Trip;

   begin
      Ada.Text_IO.Put ("Ada.Calendar test");

      --  Leap days, the end of a year, and a century that isn't a leap
      --  year (so Feb 28 is followed by March).

      Round_Trip (2000, 2, 29, 0.0);
      Round_Trip (2024, 2, 29, 86_399.5);
      Round_Trip (2026, 12, 31, 86_399.0);
      Round_Trip (2100, 2, 28, 43_200.25);
      Round_Trip (2100, 3, 1, 0.0);
      Round_Trip (1970, 1, 1, 0.0);
      Round_Trip (2399, 12, 31, 1.0);

      for Year in 1980 .. 2060 loop
         for Month in 1 .. 12 loop
            Round_Trip (Year, Month, 1, 61.0);
            Round_Trip (Year, Month, 28, 3_600.0);
         end loop;
      end loop;

      if Time_Of (2024, 2, 29) - Time_Of (2024, 2, 28) /= 86_400.0 then
         Passed := False;
         Ada.Text_IO.Put (": a day");
      end if;

      --  Every time is UTC, and it is written as such.

      if Ada.Calendar.Time_Zones.UTC_Time_Offset (Time_Of (2026, 10, 30))
        /= 0
      then
         Passed := False;
         Ada.Text_IO.Put (": time zone");
      end if;

      if Ada.Calendar.Formatting.Image (Time_Of (2026, 10, 30, 45_296.0))
        /= "2026-10-30 12:34:56"
      then
         Passed := False;
         Ada.Text_IO.Put (": image");
      end if;

      --  Clock reads the same clock as Rose.Clock, so it is between two
      --  of its readings, to within a tick, and never goes back.

      if Tests.Registry.Clock = Rose.Capabilities.Null_Capability then
         Passed := False;
         Ada.Text_IO.Put (": no clock");
      else
         if not Rose.Clock.Is_Open then
            Rose.Clock.Open (Tests.Registry.Clock);
         end if;

         declare
            Epoch  : constant Time := Time_Of (1970, 1, 1);
            Before : constant Word_64 := Rose.Clock.Realtime;
            First  : constant Time := Clock;
            Second : constant Time := Clock;
            After  : constant Word_64 := Rose.Clock.Realtime;
            Read   : constant Word_64 := To_Word (First - Epoch);
         begin
            if Read + Tick < Before or else Read > After + Tick then
               Passed := False;
               Ada.Text_IO.Put (": Clock disagrees with Rose.Clock");
            end if;
            if Second < First then
               Passed := False;
               Ada.Text_IO.Put (": Clock went backwards");
            end if;
         end;
      end if;

      return Passed;
   end Ada_Calendar_Test;

end Tests.Ada_Calendar;
