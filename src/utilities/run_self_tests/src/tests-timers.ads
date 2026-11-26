package Tests.Timers is

   function Timer_Test return Boolean;
   --  The timer server, through the Timer_Service role (#1014): an
   --  earlier timer set after a later one fires first, and not before its
   --  time; a destroyed timer doesn't make the next one fire early; a
   --  timer that is due when it is set fires at once; and the later timer
   --  still fires, at its time. Skipped without the timer or the clock.

end Tests.Timers;
