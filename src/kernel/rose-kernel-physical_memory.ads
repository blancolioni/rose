with System;

with Ada.Unchecked_Conversion;

package Rose.Kernel.Physical_Memory is

   type Allocation_Constraint is (Page_Aligned,
                                  Word_Aligned,
                                  Byte_Aligned);

   type Memory_Usage is
     (Available,            --  Allocatable region of RAM

      --  Following uses are not deallocatable
      BIOS,                 --  BIOS-owned,
      Kernel_Mem,           --  Allocated to Kernel
      Initial_System_Image, --  Initial system load image
      Kernel_Heap,          --  Kernel heap backing frame
      Page_Table,           --  User page tables
      Process_Table,        --  Kernel process table

      --  Following uses are not mergeable because they can be deleted
      Device_Owned,         --  Region owned by a device
      Device_Pages);        --  Device page space

   type Memory_Class is
     (Unused,          --  Unused (free) physical memory region
      Addressable,     --  undefined region of the addressable
                       --  physical memory range.
                       --  The initial memory region is an
                       --  Addressable range consisting of the
                       --  entire physical space.  Subsequent
                       --  regions are allocated by splitting
                       --  and defining this initial region.

      RAM,             --  Allocatable RAM region
      NVRAM,           --  Non-volatile RAM region
      ROM,             --  ROM region
      Device           --  Memory-mapped hardware device
     );

   procedure Init_Physical_Memory
     (Base  : Physical_Address;
      Bound : Physical_Address);

   function Get_User_Region_Count
     return Natural;

   procedure Get_User_Region
     (Index : Positive;
      Base  : out Rose.Addresses.Physical_Page_Address;
      Bound : out Rose.Addresses.Physical_Page_Address);

   function Allocate_Bytes
     (Constraint  : Allocation_Constraint;
      Size        : Physical_Address;
      Usage       : Memory_Usage)
      return Physical_Address;

   procedure Allocate_Region
     (Base        : in  Physical_Address;
      Bound       : in  Physical_Address;
      Class       : in  Memory_Class;
      Usage       : in  Memory_Usage;
      Success     : out Boolean);
   --  Allocate_Region: allocate a particular portion of the
   --  physical address space for the given purpose.
   --  The description defaults to the name of the
   --  memory class.

   function Available_Memory
     (Unit_Size   : in Physical_Address;
      Contiguous  : in Boolean)
      return Physical_Address;

   function To_Physical_Address is
     new Ada.Unchecked_Conversion (System.Address, Physical_Address);

   function To_Address is
     new Ada.Unchecked_Conversion (Physical_Address, System.Address);

   procedure Find_Available_RAM_Pages
     (From     : in  Rose.Addresses.Physical_Page_Address;
      Start    : out Rose.Addresses.Physical_Page_Address;
      Bound    : out Rose.Addresses.Physical_Page_Address;
      Finished : out Boolean);
   --  Searches for unallocated RAM pages starting from
   --  page From (inclusive)

   procedure Show_Regions;
   --  Show_Regions: write our region map to the console

end Rose.Kernel.Physical_Memory;
