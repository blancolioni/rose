with Rose.Addresses;
with Rose.Capabilities;

package Mem.Physical_Map is

   procedure Add_Region
     (Base, Bound    : Rose.Addresses.Physical_Page_Address;
      Map_Page_Cap   : Rose.Capabilities.Capability;
      Unmap_Page_Cap : Rose.Capabilities.Capability);

   procedure Allocate_Page
     (Page    : out Rose.Addresses.Physical_Page_Address;
      Success : out Boolean);

   procedure Deallocate_Page (Page : Rose.Addresses.Physical_Page_Address);

   function Region_Map_Page_Cap
     (Page : Rose.Addresses.Physical_Page_Address)
      return Rose.Capabilities.Capability;

   function Region_Unmap_Page_Cap
      return Rose.Capabilities.Capability;

end Mem.Physical_Map;
