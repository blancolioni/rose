package Tests.Ada_Calendar is

   function Ada_Calendar_Test return Boolean;
   --  Ada.Calendar from the run-time system (#497): dates and back,
   --  including leap days, the time zone, Formatting, and a Clock that
   --  agrees with Rose.Clock and doesn't go backwards. It runs after
   --  Tests.Clock.Clock_Test, which opens the clock.

end Tests.Ada_Calendar;
