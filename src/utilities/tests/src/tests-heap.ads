package Tests.Heap is

   function Heap_Test return Boolean;
   --  Allocators in user programs (#358): a few thousand objects of
   --  different sizes, freed and allocated again, then one object that
   --  needs the freed blocks merged and the heap grown. Every object's
   --  contents are checked.

end Tests.Heap;
