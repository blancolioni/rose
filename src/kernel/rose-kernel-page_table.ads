package Rose.Kernel.Page_Table is

   procedure Init_Page_Table;
   --  Bring the page table to a known state: nothing mapped except
   --  the kernel starting at 16#F000_0000#

   procedure Init_User_Page_Directory
     (Directory_Page : Rose.Addresses.Virtual_Page_Address);
   --  Init_User_Page_Directory is called while Kernel is mapped,
   --  thus the virtual page address argument

   procedure Activate_Page_Directory
     (Directory_Page : Rose.Addresses.Virtual_Page_Address);

   procedure Map_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address;
      Physical_Page  : Rose.Addresses.Physical_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      User           : Boolean);

   procedure Unmap_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address);

   function Mapped_Physical_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
      return Rose.Addresses.Physical_Page_Address;

   procedure Map_Kernel_Page
     (Virtual_Page   : Rose.Addresses.Virtual_Page_Address;
      Physical_Page  : Rose.Addresses.Physical_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      User           : Boolean);

   procedure Unmap_Kernel_Page
     (Virtual_Page   : Rose.Addresses.Virtual_Page_Address);

   function Kernel_Page_Directory
     return Rose.Addresses.Physical_Address;

   procedure Disable_Page_Writes
     (Directory_Page : Rose.Addresses.Virtual_Page_Address);
   --  Scan all pages referenced by directory pages referenced
   --  by this page.  If write enabled, disable and set the
   --  checkpoint-read-only flag.

   procedure Enable_Page_Writes
     (Directory_Page : Rose.Addresses.Virtual_Page_Address);
   --  Scan all pages referenced by directory pages referenced
   --  by this page.  If checkpoint-read-only set, clear it and
   --  enable page writes.
   --  Inverse of Disable_Page_Writes

   procedure Report_Mapped_Pages
     (Directory_Page : Rose.Addresses.Physical_Address);

end Rose.Kernel.Page_Table;
