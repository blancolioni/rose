package Tests.Banks is

   function Dependent_Test return Boolean;
   --  Dependent objects (ADR 0014, section 3; #448): make two sub-banks
   --  of the test bank, and register a dependent with each, with a signal
   --  cap for one of the process's own bits. Destroying the first bank
   --  makes its dependent say Destroyed and raises the signal once, and
   --  leaves the second's alone. A destroyed bank takes no dependents.
   --  Skipped without a bank or the Create_Signal cap, as when petal runs
   --  it.

   function Limit_Test return Boolean;
   --  The bank test of #479 (#517): make banks below the test bank, and
   --  check that a write past a page limit fails (a bank is charged for
   --  the pages written to its regions), that a reservation past a region
   --  limit fails, that a limit holds for the banks below it, that usage
   --  is charged to every bank above, and that destroying a bank gives
   --  back what was charged to it, and ends its regions. Skipped without
   --  a bank.

   function Reuse_Test return Boolean;
   --  A destroyed bank's caps don't reach the next bank in its slot
   --  (#967). Make a sub-bank of the test bank, destroy it while keeping
   --  its cap, and make another, which takes the slot. The new bank's
   --  index must differ from the old one's, and the old cap must not see
   --  the new bank's limits. Skipped without a bank.

end Tests.Banks;
