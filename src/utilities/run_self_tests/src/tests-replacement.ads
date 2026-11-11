package Tests.Replacement is

   function Replacement_Test return Boolean;
   --  The Replaceable interface (ADR 0032, #865), with the test server
   --  replaceable_test:
   --
   --  - launch one, and read its state twice, which is the same both
   --    times, and does not change what it serves;
   --  - get its caps, which are one cap set server Constructor, and check
   --    that it works;
   --  - launch a second one with the first's Replaceable as its Previous
   --    argument, and check that it serves the first one's name and count,
   --    and that the first one still serves the same.
   --
   --  Skipped without the Cap_Set_Service role, as when petal runs it.

end Tests.Replacement;
