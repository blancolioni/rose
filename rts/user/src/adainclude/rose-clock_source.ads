with Rose.Words;

package Rose.Clock_Source is
   pragma Preelaborate;

   --  Where Ada.Calendar gets the real time (#497). The run-time system
   --  can't reach the clock itself: a program has a cap for it from its
   --  Clock_Service role, and Rose.Clock.Open registers a reader here.

   type Reader is access function return Rose.Words.Word_64;

   procedure Set (Source : Reader);
   --  Use Source for the readings below.

   function Realtime return Rose.Words.Word_64;
   --  Nanoseconds since 1970-01-01 00:00:00 UTC, from the source. It
   --  raises Program_Error if there is none.

end Rose.Clock_Source;
