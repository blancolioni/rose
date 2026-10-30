with Rose.Words;

package Rose.Clock.Calendar is

   --  Dates for the clock's real time, in UTC (ADR 0020): the Gregorian
   --  calendar from 1970, with no leap seconds. The arithmetic needs no
   --  64-bit division, which programs don't have.

   type Weekday is
     (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

   type Date_Time is
      record
         Year       : Positive;
         Month      : Positive range 1 .. 12;
         Day        : Positive range 1 .. 31;
         Hour       : Natural range 0 .. 23;
         Minute     : Natural range 0 .. 59;
         Second     : Natural range 0 .. 59;
         Nanosecond : Natural range 0 .. 999_999_999;
      end record;

   Last_Year : constant := 2553;
   --  The last full year that a 64-bit count of nanoseconds holds.

   function Is_Valid (Time : Date_Time) return Boolean;
   --  True if Time is a date that exists, from 1970 to Last_Year.

   function To_Date_Time (Nanoseconds : Rose.Words.Word_64) return Date_Time;
   --  The date and time of a reading of real time.

   function To_Nanoseconds (Time : Date_Time) return Rose.Words.Word_64
     with Pre => Is_Valid (Time);
   --  The reading of real time for a date and time.

   function Day_Of_Week (Time : Date_Time) return Weekday
     with Pre => Is_Valid (Time);

end Rose.Clock.Calendar;
