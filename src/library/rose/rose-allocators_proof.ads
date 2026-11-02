with Rose.Allocators;

package Rose.Allocators_Proof
  with SPARK_Mode
is

   --  gnatprove proves a generic package through its instances.  These are
   --  the capacity that the store uses and a small one.

   package Small is new Rose.Allocators (4);

   package Store is new Rose.Allocators (19);

end Rose.Allocators_Proof;
