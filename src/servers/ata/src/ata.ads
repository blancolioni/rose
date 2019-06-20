with Rose.Capabilities;

package ATA is

   pragma Pure (ATA);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;
   PCI_Cap             : constant Rose.Capabilities.Capability := 3;
   Primary_IRQ_Cap     : constant Rose.Capabilities.Capability := 4;
   Secondary_IRQ_Cap   : constant Rose.Capabilities.Capability := 5;

   Command_0_Cap       : constant Rose.Capabilities.Capability := 6;
   Control_0_Cap       : constant Rose.Capabilities.Capability := 7;
   Data_0_Cap_8        : constant Rose.Capabilities.Capability := 8;
   Data_0_Cap_Read_16  : constant Rose.Capabilities.Capability := 9;
   Data_0_Cap_Write_16 : constant Rose.Capabilities.Capability := 10;

   Command_1_Cap       : constant Rose.Capabilities.Capability := 11;
   Control_1_Cap       : constant Rose.Capabilities.Capability := 12;
   Data_1_Cap_8        : constant Rose.Capabilities.Capability := 13;
   Data_1_Cap_Read_16  : constant Rose.Capabilities.Capability := 14;
   Data_1_Cap_Write_16 : constant Rose.Capabilities.Capability := 15;

   Set_Timeout_Cap     : constant Rose.Capabilities.Capability := 16;

end ATA;
