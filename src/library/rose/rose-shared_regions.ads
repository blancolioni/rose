with System;

with Rose.Interfaces.Region;

package Rose.Shared_Regions is

   --  Mapping a region into this process as a shared segment (ADR 0025).
   --  Every process that maps a page of a region shared sees the same
   --  memory, so their writes are seen at once by the others. The
   --  mappings go in the part of the address space that is kept for them,
   --  C000_0000 to D000_0000, and this package chooses where. A program
   --  that uses it puts nothing else there.

   Mapping_Error : exception;

   function Map
     (Region   : in out Rose.Interfaces.Region.Region_Object'Class;
      Writable : Boolean := True)
      return System.Address;
   --  Map all of Region, and return the address of its first page. Raise
   --  Mapping_Error if there is no room for it, or if the process has as
   --  many shared mappings as it can have.

   procedure Unmap (Address : System.Address);
   --  Remove the mapping at Address, which Map returned. Once nothing maps
   --  a page of the region, the memory server writes it back to the
   --  region if it was changed. Raise Mapping_Error if no mapping starts
   --  at Address.

end Rose.Shared_Regions;
