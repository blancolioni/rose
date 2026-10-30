package body Clock.Calendar is

   use Rose.Words;

   Seconds_Per_Day : constant := 86_400;

   --  The conversions between a date and a number of days since
   --  1970-01-01 are Howard Hinnant's days_from_civil and civil_from_days
   --  (public domain), with March as the first month of the year so that
   --  the leap day comes last.

   function Days_From_Civil
     (Year, Month, Day : Natural)
      return Natural;

   -----------------
   -- Day_Of_Week --
   -----------------

   function Day_Of_Week (Seconds : Rose.Words.Word_32) return Positive is
      Days : constant Natural := Natural (Seconds / Seconds_Per_Day);
   begin
      --  1970-01-01 was a Thursday, which is 5.

      return (Days + 4) mod 7 + 1;
   end Day_Of_Week;

   ---------------------
   -- Days_From_Civil --
   ---------------------

   function Days_From_Civil
     (Year, Month, Day : Natural)
      return Natural
   is
      Y           : constant Natural :=
                      (if Month <= 2 then Year - 1 else Year);
      Era         : constant Natural := Y / 400;
      Year_Of_Era : constant Natural := Y - Era * 400;
      Month_Index : constant Natural :=
                      (if Month > 2 then Month - 3 else Month + 9);
      Day_Of_Year : constant Natural :=
                      (153 * Month_Index + 2) / 5 + Day - 1;
      Day_Of_Era  : constant Natural :=
                      Year_Of_Era * 365 + Year_Of_Era / 4
                      - Year_Of_Era / 100 + Day_Of_Year;
   begin
      return Era * 146_097 + Day_Of_Era - 719_468;
   end Days_From_Civil;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Time : Date_Time) return Boolean is
      Days_In_Month : constant array (1 .. 12) of Natural :=
                        (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
   begin
      if Time.Year not in 2000 .. 2099
        or else Time.Month not in 1 .. 12
        or else Time.Hour > 23
        or else Time.Minute > 59
        or else Time.Second > 59
      then
         return False;
      end if;

      --  Every fourth year in 2000 .. 2099 is a leap year, including 2000.

      return Time.Day in 1 .. Days_In_Month (Time.Month)
        + (if Time.Month = 2 and then Time.Year mod 4 = 0 then 1 else 0);
   end Is_Valid;

   ------------------
   -- To_Date_Time --
   ------------------

   function To_Date_Time (Seconds : Rose.Words.Word_32) return Date_Time is
      Day_Seconds : constant Natural := Natural (Seconds mod Seconds_Per_Day);
      Z           : constant Natural :=
                      Natural (Seconds / Seconds_Per_Day) + 719_468;
      Era         : constant Natural := Z / 146_097;
      Day_Of_Era  : constant Natural := Z - Era * 146_097;
      Year_Of_Era : constant Natural :=
                      (Day_Of_Era - Day_Of_Era / 1_460
                       + Day_Of_Era / 36_524 - Day_Of_Era / 146_096) / 365;
      Day_Of_Year : constant Natural :=
                      Day_Of_Era
                      - (365 * Year_Of_Era + Year_Of_Era / 4
                         - Year_Of_Era / 100);
      Month_Index : constant Natural := (5 * Day_Of_Year + 2) / 153;
      Month       : constant Natural :=
                      (if Month_Index < 10
                       then Month_Index + 3 else Month_Index - 9);
      Year        : constant Natural :=
                      Year_Of_Era + Era * 400 + (if Month <= 2 then 1 else 0);
   begin
      return
        (Year   => Year,
         Month  => Month,
         Day    => Day_Of_Year - (153 * Month_Index + 2) / 5 + 1,
         Hour   => Day_Seconds / 3_600,
         Minute => Day_Seconds mod 3_600 / 60,
         Second => Day_Seconds mod 60);
   end To_Date_Time;

   ----------------
   -- To_Seconds --
   ----------------

   function To_Seconds (Time : Date_Time) return Rose.Words.Word_32 is
   begin
      return Word_32 (Days_From_Civil (Time.Year, Time.Month, Time.Day))
        * Seconds_Per_Day
        + Word_32 (Time.Hour * 3_600 + Time.Minute * 60 + Time.Second);
   end To_Seconds;

end Clock.Calendar;
