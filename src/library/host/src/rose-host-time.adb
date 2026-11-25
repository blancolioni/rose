with Ada.Calendar.Formatting;
with Ada.Real_Time;

package body Rose.Host.Time is

   use type Ada.Calendar.Time;
   use type Ada.Real_Time.Time;

   Start : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   function Nanoseconds (Span : Duration) return Rose.Words.Word_64;
   --  Span, which isn't negative, in nanoseconds. Duration can't hold
   --  that number, so the seconds and their fraction are converted apart.

   ---------------
   -- Monotonic --
   ---------------

   overriding function Monotonic
     (Item : in out Clock)
      return Rose.Words.Word_64
   is (Monotonic_Now);

   -------------------
   -- Monotonic_Now --
   -------------------

   function Monotonic_Now return Rose.Words.Word_64 is
   begin
      return Nanoseconds
        (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Start));
   end Monotonic_Now;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (Span : Duration) return Rose.Words.Word_64 is
      use type Rose.Words.Word_64;
      Seconds : Long_Long_Integer := Long_Long_Integer (Span);
   begin
      if Duration (Seconds) > Span then
         Seconds := Seconds - 1;
      end if;
      return Rose.Words.Word_64 (Seconds) * 1_000_000_000
        + Rose.Words.Word_64
            (Long_Long_Integer
               ((Span - Duration (Seconds)) * 1_000_000_000));
   end Nanoseconds;

   --------------
   -- Realtime --
   --------------

   overriding function Realtime
     (Item : in out Clock)
      return Rose.Words.Word_64
   is (Realtime_Now);

   ------------------
   -- Realtime_Now --
   ------------------

   function Realtime_Now return Rose.Words.Word_64 is
      Epoch : constant Ada.Calendar.Time :=
        Ada.Calendar.Formatting.Time_Of
          (Year => 1970, Month => 1, Day => 1, Hour => 0, Minute => 0,
           Second => 0, Time_Zone => 0);
   begin
      return Nanoseconds (Ada.Calendar.Clock - Epoch);
   end Realtime_Now;

end Rose.Host.Time;
