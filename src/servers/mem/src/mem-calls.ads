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
   --  Unmap Virtual from Process, and say so on the console if the kernel
   --  refuses because the page is pinned for a call in progress (#972).

   procedure Unmap
     (Process    : Rose.Objects.Object_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Unmapped   : out Boolean);
   --  Unmap Virtual from Process. Unmapped is False if the kernel refused:
   --  the page is in a buffer that Process has lent to a server in its call
   --  in progress, and the server still maps it (#972).

   procedure Load_Page
     (Physical   : Rose.Addresses.Physical_Page_Address;
      Address    : System.Address);

   procedure Unload_Page
     (Physical   : Rose.Addresses.Physical_Page_Address;
      Address    : System.Address);

   procedure Get_Process_Page
     (From    : Rose.Objects.Page_Object_Id;
      Process : out Rose.Objects.Object_Id;
      Address : out Rose.Addresses.Virtual_Page_Address);

   function Get_Page_Object
     (Process : Rose.Objects.Object_Id;
      Address : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Page_Object_Id;

end Mem.Calls;
