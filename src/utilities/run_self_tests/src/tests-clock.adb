with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Clock.Calendar;
with Rose.Words;

with Tests.Registry;

package body Tests.Clock is

   use Rose.Words;
   use type Rose.Capabilities.Capability;
   use type Rose.Clock.Calendar.Weekday;

   Tick : constant Word_64 := 10_000_150;
   --  Nanoseconds in a tick.

   function Difference (A, B : Word_64) return Word_64;
   --  The distance between A and B.

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   function Nanoseconds (Seconds : Word_64) return Word_64;

   -------------------
   -- Calendar_Test --
   -------------------

   function Calendar_Test return Boolean is
      use Rose.Clock.Calendar;

      Passed : Boolean := True;

      procedure Check
        (Seconds : Word_64;
         Time    : Date_Time;
         Day     : Weekday);
      --  Seconds is Time, which falls on Day.

      -----------
      -- Check --
      -----------

      procedure Check
        (Seconds : Word_64;
         Time    : Date_Time;
         Day     : Weekday)
      is
      begin
         if not Is_Valid (Time)
           or else To_Nanoseconds (Time) /= Nanoseconds (Seconds)
           or else To_Date_Time (Nanoseconds (Seconds)) /= Time
           or else Day_Of_Week (Time) /= Day
         then
            Passed := False;
            Ada.Text_IO.Put
              (": " & Positive'Image (Time.Year) & Positive'Image (Time.Month)
               & Positive'Image (Time.Day));
         end if;
      end Check;

      Seconds : Word_64 := 0;
   begin
      Ada.Text_IO.Put ("Calendar test");

      Check (0, (1970, 1, 1, 0, 0, 0, 0), Thursday);
      Check (951_825_600, (2000, 2, 29, 12, 0, 0, 0), Tuesday);
      Check (1_709_164_800, (2024, 2, 29, 0, 0, 0, 0), Thursday);
      Check (1_793_363_696, (2026, 10, 30, 12, 34, 56, 0), Friday);
      Check (2_147_483_647, (2038, 1, 19, 3, 14, 7, 0), Tuesday);

      --  2100 is not a leap year, and 2400 is.

      Check (4_107_542_399, (2100, 2, 28, 23, 59, 59, 0), Sunday);
      Check (4_107_542_400, (2100, 3, 1, 0, 0, 0, 0), Monday);
      Check (13_574_563_200, (2400, 2, 29, 0, 0, 0, 0), Tuesday);
      Check (18_429_292_799, (2553, 12, 31, 23, 59, 59, 0), Monday);

      --  A fraction of a second is kept.

      if To_Date_Time (Nanoseconds (1) + 123_456_789).Nanosecond
        /= 123_456_789
      then
         Passed := False;
         Ada.Text_IO.Put (": nanoseconds");
      end if;

      if Is_Valid ((2001, 2, 29, 0, 0, 0, 0))
        or else Is_Valid ((2100, 2, 29, 0, 0, 0, 0))
        or else Is_Valid ((2026, 4, 31, 0, 0, 0, 0))
        or else Is_Valid ((1969, 12, 31, 0, 0, 0, 0))
      then
         Passed := False;
         Ada.Text_IO.Put (": dates that don't exist");
      end if;

      --  Every seven days or so, from 1970 to 2553, a date and back.

      while Seconds < 18_429_292_799 loop
         if To_Nanoseconds (To_Date_Time (Nanoseconds (Seconds)))
           /= Nanoseconds (Seconds)
         then
            Passed := False;
            Ada.Text_IO.Put (": round trip");
            exit;
         end if;
         Seconds := Seconds + 604_800 + 3_601;
      end loop;

      return Passed;
   end Calendar_Test;

   ----------------
   -- Clock_Test --
   ----------------

   function Clock_Test return Boolean is
      Year_2000 : constant Word_64 := Nanoseconds (946_684_800);
      --  The machine's clock is later than that.
   begin
      Ada.Text_IO.Put ("Clock test");

      if Tests.Registry.Clock = Rose.Capabilities.Null_Capability then
         return Fail ("no clock");
      end if;
      Rose.Clock.Open (Tests.Registry.Clock);

      declare
         Mono_1  : constant Word_64 := Rose.Clock.Monotonic;
         Real_1  : constant Word_64 := Rose.Clock.Realtime;
         Mono    : Word_64 := Mono_1;
         Spins   : Natural := 0;
      begin
         if Real_1 < Year_2000 then
            return Fail ("the real time is before 2000");
         end if;

         --  Wait for a fifth of a second of the clock's own time. It
         --  never goes backwards while we do.

         while Mono - Mono_1 < 200_000_000 loop
            declare
               Next : constant Word_64 := Rose.Clock.Monotonic;
            begin
               if Next < Mono then
                  return Fail ("the monotonic time went backwards");
               end if;
               Mono := Next;
            end;
            Spins := Spins + 1;
            if Spins > 1_000_000 then
               return Fail ("the monotonic time doesn't advance");
            end if;
         end loop;

         declare
            Mono_2 : constant Word_64 := Rose.Clock.Monotonic;
            Real_2 : constant Word_64 := Rose.Clock.Realtime;
         begin
            if Mono_2 - Mono_1 > 2_000_000_000 then
               return Fail ("the monotonic time jumped");
            elsif Real_2 < Real_1 then
               return Fail ("the real time went backwards");
            elsif Difference (Real_2 - Real_1, Mono_2 - Mono_1) > 2 * Tick
            then
               return Fail ("the real and monotonic times disagree");
            end if;
         end;
      end;

      if not Rose.Clock.Realtime_Is_Trusted then
         Ada.Text_IO.Put (" (the real time is not trusted)");
      end if;
      return True;
   end Clock_Test;

   ----------------
   -- Difference --
   ----------------

   function Difference (A, B : Word_64) return Word_64
   is (if A > B then A - B else B - A);

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Message);
      return False;
   end Fail;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (Seconds : Word_64) return Word_64
   is (Seconds * 1_000_000_000);

end Tests.Clock;
