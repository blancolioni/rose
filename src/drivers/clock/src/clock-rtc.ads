with Clock.Calendar;

package Clock.RTC is

   --  The CMOS real-time clock: a time of day in UTC, to the second.

   procedure Read
     (Time  : out Clock.Calendar.Date_Time;
      Valid : out Boolean);
   --  The time that the RTC has. Valid is False if its battery bit is
   --  clear, or its fields are out of range, and then Time is not the
   --  time.

   procedure Write (Time : Clock.Calendar.Date_Time)
     with Pre => Clock.Calendar.Is_Valid (Time);
   --  Set the RTC. The clock starts counting from Time at the moment
   --  that this returns, so call it on a whole second.

end Clock.RTC;
