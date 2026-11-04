package Tests.Signals is

   function Signal_Test return Boolean;
   --  Signals (ADR 0006, #708): make a signal cap for one of the
   --  process's own bits, invoke it twice, which never waits, and receive:
   --  the bit's handler is called once, and another bit's not at all.
   --  Skipped without the Create_Signal cap, as when petal runs it.

end Tests.Signals;
