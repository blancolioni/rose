with Rose.Addresses;
with Rose.Objects;

package Mem.Virtual_Map is

   procedure Reclaim
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean);

   procedure Map (Process       : Rose.Objects.Object_Id;
                  Physical_Page : Rose.Addresses.Physical_Page_Address;
                  Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
                  Readable      : Boolean;
                  Writable      : Boolean;
                  Executable    : Boolean);

   procedure Remove_All
     (Process : Rose.Objects.Capability_Identifier);

end Mem.Virtual_Map;
