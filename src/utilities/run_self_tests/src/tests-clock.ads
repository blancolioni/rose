package Tests.Clock is

   function Calendar_Test return Boolean;
   --  Rose.Clock.Calendar: known dates, leap days, and a round trip over
   --  every year it covers.

   function Clock_Test return Boolean;
   --  The system's clock, through the Clock_Service role (ADR 0020): its
   --  readings advance, together, and don't go backwards.

end Tests.Clock;
