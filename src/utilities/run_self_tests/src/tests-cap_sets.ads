package Tests.Cap_Sets is

   function Bank_Set_Test return Boolean;
   --  Cap sets made in a bank (#849): make two sub-banks of the tests'
   --  bank, a set in each, and put a cap in each. Destroy the first bank,
   --  and check that its set no longer answers, that the other still
   --  holds its cap, and that no set can be made in the destroyed bank.
   --  Skipped without the Cap_Set_Service role or a bank, as when petal
   --  runs it.

   function Cap_Set_Test return Boolean;
   --  The userland cap set server (#825), through the Cap_Set_Service
   --  role: fill a set with copies of one cap until Append raises, and
   --  check that it held thousands, that the failed Append added none,
   --  and that a cap comes back out of it. Release it, and check that it
   --  can't be used, and that a new set can be filled as far again, which
   --  it can only be if the released set's caps were deleted. Skipped
   --  without the role, as when petal runs it.

end Tests.Cap_Sets;
