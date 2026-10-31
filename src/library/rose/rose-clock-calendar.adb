package body Rose.Clock.Calendar is

   use Rose.Words;

   Nanoseconds_Per_Second : constant := 1_000_000_000;
   Seconds_Per_Day        : constant := 86_400;

   --  The conversions between a date and a number of days since
   --  1970-01-01 are Howard Hinnant's days_from_civil and civil_from_days
   --  (public domain), with March as the first month of the year so that
   --  the leap day comes last.

   function Days_From_Civil (Time : Date_Time) return Natural;

   procedure Divide
     (Value     : Word_64;
      Divisor   : Word_32;
      Quotient  : out Word_64;
      Remainder : out Word_32);
   --  Quotient and Remainder of Value / Divisor, by shifting and
   --  subtracting.

   function Is_Leap_Year (Year : Positive) return Boolean;

   -----------------
   -- Day_Of_Week --
   -----------------

   function Day_Of_Week (Time : Date_Time) return Weekday is
   begin

      --  1970-01-01 was a Thursday.

      return Weekday'Val ((Days_From_Civil (Time) + 4) mod 7);
   end Day_Of_Week;

   ---------------------
   -- Days_From_Civil --
   ---------------------

   function Days_From_Civil (Time : Date_Time) return Natural is
      Y           : constant Natural :=
                      (if Time.Month <= 2 then Time.Year - 1 else Time.Year);
      Era         : constant Natural := Y / 400;
      Year_Of_Era : constant Natural := Y - Era * 400;
      Month_Index : constant Natural :=
                      (if Time.Month > 2 then Time.Month - 3
                       else Time.Month + 9);
      Day_Of_Year : constant Natural :=
                      (153 * Month_Index + 2) / 5 + Time.Day - 1;
      Day_Of_Era  : constant Natural :=
                      Year_Of_Era * 365 + Year_Of_Era / 4
                      - Year_Of_Era / 100 + Day_Of_Year;
   begin
      return Era * 146_097 + Day_Of_Era - 719_468;
   end Days_From_Civil;

   ------------
   -- Divide --
   ------------

   procedure Divide
     (Value     : Word_64;
      Divisor   : Word_32;
      Quotient  : out Word_64;
      Remainder : out Word_32)
   is
      Mask : Word_64 := 2 ** 63;
      Rest : Word_64 := 0;
   begin
      Quotient := 0;
      while Mask /= 0 loop
         Rest := Rest * 2 + (if (Value and Mask) /= 0 then 1 else 0);
         Quotient := Quotient * 2;
         if Rest >= Word_64 (Divisor) then
            Rest := Rest - Word_64 (Divisor);
            Quotient := Quotient + 1;
         end if;
         Mask := Mask / 2;
      end loop;
      Remainder := Word_32 (Rest);
   end Divide;

   ------------------
   -- Is_Leap_Year --
   ------------------

   function Is_Leap_Year (Year : Positive) return Boolean is
   begin
      return Year mod 4 = 0
        and then (Year mod 100 /= 0 or else Year mod 400 = 0);
   end Is_Leap_Year;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Time : Date_Time) return Boolean is
      Days_In_Month : constant array (1 .. 12) of Positive :=
                        (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
   begin
      return Time.Year in 1970 .. Last_Year
        and then Time.Day <= Days_In_Month (Time.Month)
          + (if Time.Month = 2 and then Is_Leap_Year (Time.Year) then 1
             else 0);
   end Is_Valid;

   ------------------
   -- To_Date_Time --
   ------------------

   function To_Date_Time (Nanoseconds : Rose.Words.Word_64) return Date_Time is
      Seconds     : Word_64;
      Nanosecond  : Word_32;
      Days        : Word_64;
      Day_Seconds : Word_32;
   begin
      Divide (Nanoseconds, Nanoseconds_Per_Second, Seconds, Nanosecond);
      Divide (Seconds, Seconds_Per_Day, Days, Day_Seconds);

      declare
         Z           : constant Natural := Natural (Days) + 719_468;
         Era         : constant Natural := Z / 146_097;
         Day_Of_Era  : constant Natural := Z - Era * 146_097;
         Year_Of_Era : constant Natural :=
                         (Day_Of_Era - Day_Of_Era / 1_460
                          + Day_Of_Era / 36_524 - Day_Of_Era / 146_096)
                         / 365;
         Day_Of_Year : constant Natural :=
                         Day_Of_Era
                         - (365 * Year_Of_Era + Year_Of_Era / 4
                            - Year_Of_Era / 100);
         Month_Index : constant Natural := (5 * Day_Of_Year + 2) / 153;
         Month       : constant Positive :=
                         (if Month_Index < 10
                          then Month_Index + 3 else Month_Index - 9);
         Seconds_Now : constant Natural := Natural (Day_Seconds);
      begin
         return
           (Year       => Year_Of_Era + Era * 400
                          + (if Month <= 2 then 1 else 0),
            Month      => Month,
            Day        => Day_Of_Year - (153 * Month_Index + 2) / 5 + 1,
            Hour       => Seconds_Now / 3_600,
            Minute     => Seconds_Now mod 3_600 / 60,
            Second     => Seconds_Now mod 60,
            Nanosecond => Natural (Nanosecond));
      end;
   end To_Date_Time;

   ----------------
   -- To_Elapsed --
   ----------------

   function To_Elapsed
     (Nanoseconds : Rose.Words.Word_64)
      return Elapsed_Time
   is
      Total_Seconds : Word_64;
      Nanosecond    : Word_32;
      Days          : Word_64;
      Day_Seconds   : Word_32;
   begin
      Divide (Nanoseconds, Nanoseconds_Per_Second, Total_Seconds, Nanosecond);
      Divide (Total_Seconds, Seconds_Per_Day, Days, Day_Seconds);

      return
        (Days       => Natural (Days),
         Hours      => Natural (Day_Seconds) / 3_600,
         Minutes    => Natural (Day_Seconds) mod 3_600 / 60,
         Seconds    => Natural (Day_Seconds) mod 60,
         Nanosecond => Natural (Nanosecond));
   end To_Elapsed;

   --------------------
   -- To_Nanoseconds --
   --------------------

   function To_Nanoseconds (Time : Date_Time) return Rose.Words.Word_64 is
      Seconds : constant Word_64 :=
                  Word_64 (Days_From_Civil (Time)) * Seconds_Per_Day
                  + Word_64 (Time.Hour * 3_600 + Time.Minute * 60
                             + Time.Second);
   begin
      return Seconds * Nanoseconds_Per_Second + Word_64 (Time.Nanosecond);
   end To_Nanoseconds;

end Rose.Clock.Calendar;
