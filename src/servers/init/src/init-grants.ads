--  Caps for hardware, derived from init's root caps
--  (Rose.Init_Capabilities), and init's record of which program each
--  port, interrupt and device page went to.  Hardware that has gone to
--  one program is refused to any other (#115).

with Rose.Capabilities;
with Rose.Words;

with Rose.Interfaces.Devices.Pci_Device.Client;

package Init.Grants is

   type Owner_Id is new Natural;
   --  Who a grant is for: a boot server's object id, or an installed
   --  program's number from Next_Owner.

   No_Owner : constant Owner_Id := 0;
   --  Grants to No_Owner are not recorded, and can overlap any other.

   function Next_Owner return Owner_Id;
   --  A new owner for an installed program.

   function Ports
     (Owner    : Owner_Id;
      Endpoint : Rose.Words.Word;
      Size     : Rose.Words.Word;
      First    : Rose.Words.Word;
      Last     : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A port cap for First .. Last, or the null cap if another owner has
   --  any of those ports, or the kernel refuses.  Endpoint and Size are
   --  from Rose.Init_Capabilities.

   function Interrupt
     (Owner  : Owner_Id;
      Vector : Rose.Words.Word)
      return Rose.Capabilities.Capability;

   --  PCI devices (#292): a driver asks for a device by its vendor and
   --  device ids, and then for its ports and its interrupt.

   function PCI_Device
     (Owner  : Owner_Id;
      Bus    : Rose.Capabilities.Capability;
      Vendor : Rose.Words.Word;
      Device : Rose.Words.Word;
      Index  : Rose.Words.Word;
      Client : out Rose.Interfaces.Devices.Pci_Device.Client
                     .Pci_Device_Client)
      return Rose.Capabilities.Capability;
   --  The Index'th device with these ids, counting from 0, from Bus, the
   --  pci server's PCI_Bus: its interface cap, which can write the
   --  device's command register, and in Client a client of it for init.
   --  The null cap if pci has no such device, or another owner has it.

   function PCI_Ports
     (Owner    : Owner_Id;
      Device   : Rose.Interfaces.Devices.Pci_Device.Client
                   .Pci_Device_Client;
      BAR      : Rose.Words.Word;
      Endpoint : Rose.Words.Word;
      Size     : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A range port cap, Endpoint and Size as for Ports, for the I/O ports
   --  of Device's base address register BAR, or the null cap if BAR isn't
   --  an I/O BAR, or Ports refuses.

   function PCI_Interrupt
     (Owner  : Owner_Id;
      Device : Rose.Interfaces.Devices.Pci_Device.Client.Pci_Device_Client)
      return Rose.Capabilities.Capability;
   --  A cap for Device's interrupt, as Interrupt gives, or the null cap if
   --  it has none.
   --  A cap that installs a handler for Vector, or the null cap if
   --  another owner has it, or the kernel refuses.

   function Device_Page
     (Owner : Owner_Id;
      Page  : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A cap that maps physical page Page at the same address, or the
   --  null cap if another owner has it, or the kernel refuses.

   function Device_Pages
     (Owner : Owner_Id;
      First : Rose.Words.Word;
      Count : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  One cap that maps physical pages First .. First + Count - 1 at their
   --  own addresses, for DMA (#291), or the null cap if Count is 0,
   --  another owner has any of them, or the kernel refuses: the kernel
   --  grants only pages in the first megabyte that aren't RAM.

   function Framebuffer
     (Owner : Owner_Id)
      return Rose.Capabilities.Capability;
   --  A cap that maps the boot loader's linear framebuffer at an address
   --  in the device window, or the null cap if there is no RGB
   --  framebuffer below 4 GB, another owner has any of its pages, or the
   --  kernel refuses (#582).

   function Has_Framebuffer return Boolean;
   --  True if the boot loader gave a 32-bit RGB framebuffer below 4 GB,
   --  which the screen driver can use (#582).

   function Boot_Tag
     (Tag : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A cap that reads the boot loader's tag of type Tag, or the null cap
   --  if the kernel refuses.  Reading a tag conflicts with nothing, so it
   --  is not recorded.

   function Bank
     (Pages   : Rose.Words.Word;
      Regions : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A new sub-bank of the system bank (ADR 0014), limited to Pages
   --  pages and Regions regions, or the null cap if there is no system
   --  bank or either limit is zero, which would be no limit.  A bank is
   --  storage, not hardware, so it is not recorded.

   procedure System_Bank
     (Storage : Rose.Capabilities.Capability;
      Bank    : out Rose.Capabilities.Capability;
      Reserve : out Rose.Capabilities.Capability);
   --  A new sub-bank of Storage's root bank, with no limits: the system
   --  bank (ADR 0014, section 2; #450).  Bank is its Bank interface,
   --  which init gives elf and exec to launch against, and Reserve its
   --  Reserve_Storage endpoint, which the installer reserves from.
   --  Null caps if Storage is null.  Init makes it once, at the install:
   --  the processes launched against it keep their caps across a
   --  restore, and a restored init launches nothing.

end Init.Grants;
