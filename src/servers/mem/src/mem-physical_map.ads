with Rose.Addresses;
with Rose.Capabilities;
with Rose.Words;

package Mem.Physical_Map is

   procedure Add_Region
     (Base, Bound    : Rose.Addresses.Physical_Page_Address;
      Map_Page_Cap   : Rose.Capabilities.Capability;
      Unmap_Page_Cap : Rose.Capabilities.Capability;
      Set_Page_Cap   : Rose.Capabilities.Capability);

   procedure Allocate_Page
     (Page    : out Rose.Addresses.Physical_Page_Address;
      Success : out Boolean);

   procedure Deallocate_Page (Page : Rose.Addresses.Physical_Page_Address);

   procedure Take_Memory
     (Size   : Rose.Addresses.Physical_Bytes;
      Start  : out Rose.Addresses.Physical_Address;
      Amount : out Rose.Addresses.Physical_Bytes);
   --  Give contiguous physical memory to the kernel's heap, from the free
   --  pages, which are no longer given to processes: a run of Size bytes,
   --  or else the longest free run there is (#1051). Amount is zero if no
   --  page is free.

   function Region_Map_Page_Cap
     (Page : Rose.Addresses.Physical_Page_Address)
      return Rose.Capabilities.Capability;

   function Region_Unmap_Page_Cap
      return Rose.Capabilities.Capability;

   function Region_Set_Page_Cap
     (Page : Rose.Addresses.Physical_Page_Address)
      return Rose.Capabilities.Capability;

   function Available_Pages return Rose.Words.Word;
   function Allocated_Pages return Rose.Words.Word;

end Mem.Physical_Map;
