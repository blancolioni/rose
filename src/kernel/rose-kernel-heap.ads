package Rose.Kernel.Heap is

   --  Kernel heap manager

   procedure Initialise_Heap
     (Required : Physical_Bytes);

   --  Allocate Count bytes, which are all zeroed.
   function Allocate
     (Count : Virtual_Bytes;
      Align : Virtual_Bytes := 4)
     return Rose.Addresses.Virtual_Address;

   --  Page at a time allocation (also zeroed).
   --  Both versions are identical, except for the address they return
   function Allocate_Page return Rose.Addresses.Virtual_Page_Address;
   function Allocate_Page return Rose.Addresses.Physical_Page_Address;

   procedure Free (Addr : Rose.Addresses.Virtual_Address);

   function Push_IPC_Page return Rose.Addresses.Virtual_Page_Address;
   procedure Pop_IPC_Page;

   procedure Get_Status
     (Allocated : out Physical_Bytes;
      Available : out Physical_Bytes);

end Rose.Kernel.Heap;
