with System.Storage_Elements;

package Rose.CRTL is

   function Allocate
     (Size : System.Storage_Elements.Storage_Count)
     return System.Address;

   pragma Export (C, Allocate, "malloc");

   function Reallocate
     (Ptr  : System.Address;
      Size : System.Storage_Elements.Storage_Count)
     return System.Address;

   pragma Export (C, Reallocate, "realloc");

   procedure Deallocate
     (Ptr : System.Address);

   pragma Export (C, Deallocate, "free");

end Rose.CRTL;
