package Tests.Regions is

   function Put_Limit_Test return Boolean;
   --  A Put that the store has no space for fails (#945). Reserve a
   --  region of four pages from a sub-bank of the test bank that is
   --  limited to two. The store charges a page when it is first written,
   --  so two pages can be written and a third can't: its Put raises, and
   --  the page still reads as zeroes. Writing zeroes to a page gives its
   --  space back, and then the third page can be written. Skipped without
   --  a bank, as when petal runs it.

   function Reuse_Test return Boolean;
   --  A released region's caps don't reach the next region in its slot
   --  (#967). Reserve a region and write a page, release it while
   --  keeping its cap, and reserve another, which takes the slot. The
   --  old cap must not see the new region's range, write its page, or
   --  read it. Skipped without a bank.

end Tests.Regions;
