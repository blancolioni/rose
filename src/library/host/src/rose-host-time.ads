with Rose.Capabilities;
with Rose.Words;

with Rose.Interfaces.Time.Clock;

package Rose.Host.Time is

   --  The clock, over the host's (ADR 0020).

   type Clock is limited new Rose.Interfaces.Time.Clock.Clock_Object
     with null record;

   procedure Open
     (Item : in out Clock;
      Name : String)
   is null;
   --  The backing of the Clock_Service role: the host's clock. There is
   --  nothing to open.

   overriding function Clock_Cap
     (Item : in out Clock)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   overriding function Monotonic
     (Item : in out Clock)
      return Rose.Words.Word_64;

   overriding function Realtime
     (Item : in out Clock)
      return Rose.Words.Word_64;

   overriding function Realtime_Is_Trusted
     (Item : in out Clock)
      return Boolean
   is (True);

   function Monotonic_Now return Rose.Words.Word_64;
   --  Nanoseconds since the program started, from Ada.Real_Time.

   function Realtime_Now return Rose.Words.Word_64;
   --  Nanoseconds since 1970-01-01 00:00:00 UTC, from Ada.Calendar.

end Rose.Host.Time;
