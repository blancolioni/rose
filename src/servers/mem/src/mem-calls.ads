with System;

with Rose.Addresses;
with Rose.Objects;

private package Mem.Calls is

   procedure Load_Memory_Map;

   procedure Map
     (Process    : Rose.Objects.Object_Id;
      Physical   : Rose.Addresses.Physical_Page_Address;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Readable   : Boolean;
      Writeable  : Boolean;
      Executable : Boolean);

   procedure Unmap
     (Process    : Rose.Objects.Object_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address);

   procedure Load_Page
     (Physical   : Rose.Addresses.Physical_Page_Address;
      Address    : System.Address);

   procedure Unload_Page
     (Physical   : Rose.Addresses.Physical_Page_Address;
      Address    : System.Address);

end Mem.Calls;
