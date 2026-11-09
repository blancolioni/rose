package Tests.Banks is

   function Dependent_Test return Boolean;
   --  Dependent objects (ADR 0014, section 3; #448): make two sub-banks
   --  of the test bank, and register a dependent with each, with a signal
   --  cap for one of the process's own bits. Destroying the first bank
   --  makes its dependent say Destroyed and raises the signal once, and
   --  leaves the second's alone. A destroyed bank takes no dependents.
   --  Skipped without a bank or the Create_Signal cap, as when petal runs
   --  it.

end Tests.Banks;
