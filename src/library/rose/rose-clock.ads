with Rose.Capabilities;
with Rose.Words;

package Rose.Clock is

   --  The system's clock (ADR 0020), through a cap to the Clock
   --  interface. A program gets the cap from its Clock_Service role
   --  (interfaces/clock_service.idl), and opens it once. Rose.Clock.
   --  Calendar converts a reading of real time to and from a date.

   procedure Open (Clock : Rose.Capabilities.Capability);
   --  Use Clock, a cap for the Clock interface, for the readings below.

   function Is_Open return Boolean;

   function Monotonic return Rose.Words.Word_64
     with Pre => Is_Open;
   --  Nanoseconds since the system was first started, not counting the
   --  time that it was off. It never goes backwards, including across a
   --  restore. Its resolution is a tick, 10 ms.

   function Realtime return Rose.Words.Word_64
     with Pre => Is_Open;
   --  Nanoseconds since 1970-01-01 00:00:00 UTC, without leap seconds.

   function Realtime_Is_Trusted return Boolean
     with Pre => Is_Open;
   --  False if Realtime is known to be a guess, because the machine's
   --  clock was never set, or the system was restored onto an earlier
   --  one.

   --  These raise Program_Error if the clock can't be reached.

end Rose.Clock;
