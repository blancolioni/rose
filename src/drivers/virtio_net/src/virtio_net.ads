with Rose.Capabilities;

package Virtio_Net is

   --  The driver for QEMU's virtio network card, through the legacy
   --  interface in I/O port BAR 0 (#295). It serves Network_Device.

   pragma Pure (Virtio_Net);

   --  The caps of virtio_net.driver, in its order.

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Command_Line_Cap    : constant Rose.Capabilities.Capability := 3;
   DMA_Pages_Cap       : constant Rose.Capabilities.Capability := 4;
   PCI_Device_Cap      : constant Rose.Capabilities.Capability := 5;
   Port_In_8_Cap       : constant Rose.Capabilities.Capability := 6;
   Port_In_16_Cap      : constant Rose.Capabilities.Capability := 7;
   Port_In_32_Cap      : constant Rose.Capabilities.Capability := 8;
   Port_Out_8_Cap      : constant Rose.Capabilities.Capability := 9;
   Port_Out_16_Cap     : constant Rose.Capabilities.Capability := 10;
   Port_Out_32_Cap     : constant Rose.Capabilities.Capability := 11;
   Interrupt_Cap       : constant Rose.Capabilities.Capability := 12;

   Console_Writer_Cap  : constant Rose.Capabilities.Capability := 13;
   --  The persistent console, which the installer gives after the
   --  driver's own caps (Requires_Console). Without a card, init refuses
   --  the card's eight requests and the console is cap 5, so this slot is
   --  empty: that is how the driver knows that there is no card.

   First_DMA_Page : constant := 16#10#;
   DMA_Page_Count : constant := 16;
   --  The pages of device-pages in virtio_net.driver

end Virtio_Net;
