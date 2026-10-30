with Rose.Words;

package Clock.Calendar is

   --  The calendar arithmetic for the RTC, for the years 2000 to 2099,
   --  which is all that the RTC can hold. In those years a number of
   --  seconds since 1970 fits in 32 bits.

   type Date_Time is
      record
         Year   : Natural;
         Month  : Natural;
         Day    : Natural;
         Hour   : Natural;
         Minute : Natural;
         Second : Natural;
      end record;

   First_Second : constant Rose.Words.Word_32 := 946_684_800;
   --  2000-01-01 00:00:00

   Last_Second  : constant Rose.Words.Word_32 := 4_102_444_799;
   --  2099-12-31 23:59:59

   function Is_Valid (Time : Date_Time) return Boolean;
   --  True if Time has a year from 2000 to 2099 and fields that exist.

   function To_Seconds (Time : Date_Time) return Rose.Words.Word_32
     with Pre => Is_Valid (Time);
   --  Seconds since 1970-01-01 00:00:00, without leap seconds.

   function To_Date_Time (Seconds : Rose.Words.Word_32) return Date_Time
     with Pre => Seconds in First_Second .. Last_Second;

   function Day_Of_Week (Seconds : Rose.Words.Word_32) return Positive
     with Pre => Seconds in First_Second .. Last_Second;
   --  1 for Sunday to 7 for Saturday, as the RTC counts them.

end Clock.Calendar;
