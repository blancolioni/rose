with Ada.Unchecked_Conversion;

with Rose.Clock_Source;
with Rose.Words;

package body System.OS_Primitives is

   pragma Assert (Duration'Size = 64 and then Duration'Small = 1.0E-9);
   --  So that a count of nanoseconds is the representation of a Duration.

   function To_Duration is new Ada.Unchecked_Conversion
     (Rose.Words.Word_64, Duration);

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
   begin
      return To_Duration (Rose.Clock_Source.Realtime);
   end Clock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay (Time : Duration; Mode : Integer) is
      pragma Unreferenced (Time, Mode);
   begin
      raise Program_Error with "a delay is not supported yet";
   end Timed_Delay;

end System.OS_Primitives;
