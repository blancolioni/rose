with System.Storage_Elements;

package Rose.Unsaved_Memory is

   --  Memory that no checkpoint saves (#1132), for what a persistent
   --  program holds but mustn't keep, such as a password while it is
   --  typed. It is a segment of the process that isn't persistent
   --  (Segment.Add_Nonpersistent_Segment): its pages start as zeros, are
   --  never written to the log, and are zeros again after a restore. They
   --  are never paged out either, so the memory is meant to be small.
   --
   --  The segments go in the part of the address space that is kept for
   --  them, D000_0000 to D100_0000, above the shared segments
   --  (Rose.Shared_Regions) and well below the stack, which ends at
   --  E000_0000, and this package chooses where. Below the shared
   --  segments is the heap's, which may grow up to them (#759). A program
   --  that uses this package puts nothing else there.

   Mapping_Error : exception;

   function Allocate
     (Size : System.Storage_Elements.Storage_Count)
      return System.Address;
   --  Map a segment of at least Size bytes, a whole number of pages, and
   --  return the address of its first byte. Raise Mapping_Error if Size
   --  is zero, if there is no room for it, or if the process has as many
   --  of these segments as it can have.

   procedure Release (Address : System.Address);
   --  Remove the segment at Address, which Allocate returned, and drop
   --  what it holds. Raise Mapping_Error if no segment starts at Address.

end Rose.Unsaved_Memory;
