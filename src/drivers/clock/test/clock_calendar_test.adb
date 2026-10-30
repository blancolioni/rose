with Ada.Text_IO;

with Rose.Words;

with Clock.Calendar;

--  Host test of Clock.Calendar (make test in src/drivers/clock). The
--  reference values are from Python's datetime.

procedure Clock_Calendar_Test is

   use Rose.Words;
   use Clock.Calendar;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Message : String);

   procedure Check_Date
     (Year, Month, Day, Hour, Minute, Second : Natural;
      Seconds : Word_32;
      Weekday : Positive);

   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Message);
      end if;
   end Check;

   procedure Check_Date
     (Year, Month, Day, Hour, Minute, Second : Natural;
      Seconds : Word_32;
      Weekday : Positive)
   is
      Time : constant Date_Time :=
               (Year, Month, Day, Hour, Minute, Second);
      Name : constant String :=
               Natural'Image (Year) & Natural'Image (Month)
               & Natural'Image (Day);
   begin
      Check (Is_Valid (Time), "valid" & Name);
      Check (To_Seconds (Time) = Seconds, "to seconds" & Name);
      Check (To_Date_Time (Seconds) = Time, "to date" & Name);
      Check (Day_Of_Week (Seconds) = Weekday, "weekday" & Name);
   end Check_Date;

begin
   Check_Date (2000, 1, 1, 0, 0, 0, 946_684_800, 7);
   Check_Date (2000, 2, 29, 12, 0, 0, 951_825_600, 3);
   Check_Date (2024, 2, 29, 0, 0, 0, 1_709_164_800, 5);
   Check_Date (2026, 10, 30, 12, 34, 56, 1_793_363_696, 6);
   Check_Date (2038, 1, 19, 3, 14, 7, 2_147_483_647, 3);
   Check_Date (2099, 12, 31, 23, 59, 59, 4_102_444_799, 5);

   Check (not Is_Valid ((1999, 12, 31, 23, 59, 59)), "1999");
   Check (not Is_Valid ((2100, 1, 1, 0, 0, 0)), "2100");
   Check (not Is_Valid ((2001, 2, 29, 0, 0, 0)), "2001 is not a leap year");
   Check (not Is_Valid ((2026, 13, 1, 0, 0, 0)), "month 13");
   Check (not Is_Valid ((2026, 4, 31, 0, 0, 0)), "April 31");
   Check (not Is_Valid ((2026, 4, 30, 24, 0, 0)), "hour 24");

   --  Every day in range, at three times, round trips, and the seconds
   --  count up without gaps.

   declare
      Seconds : Word_32 := First_Second;
   begin
      while Seconds <= Last_Second - 86_400 loop
         declare
            Midnight : constant Word_32 := Seconds - Seconds mod 86_400;
         begin
            Check (To_Seconds (To_Date_Time (Seconds)) = Seconds,
                   "round trip" & Word_32'Image (Seconds));
            Check (To_Date_Time (Midnight + 86_399).Day
                   = To_Date_Time (Midnight).Day,
                   "last second of the day" & Word_32'Image (Seconds));
            Check (To_Date_Time (Midnight + 86_400).Day
                   /= To_Date_Time (Midnight).Day,
                   "next day" & Word_32'Image (Seconds));
         end;
         Seconds := Seconds + 86_400 + 3_601;
         exit when Failures > 10;
      end loop;
   end;

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("clock calendar: ok");
   else
      Ada.Text_IO.Put_Line ("clock calendar:" & Natural'Image (Failures)
                            & " failed");
      raise Program_Error;
   end if;
end Clock_Calendar_Test;
