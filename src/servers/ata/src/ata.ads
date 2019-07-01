with Rose.Capabilities;

package ATA is

   Create_Endpoint_Cap : Rose.Capabilities.Capability := 1;
   Take_Next_Cap       : Rose.Capabilities.Capability := 2;

   Console_Cap         : Rose.Capabilities.Capability;
   PCI_Cap             : Rose.Capabilities.Capability;
   Primary_IRQ_Cap     : Rose.Capabilities.Capability;
   Secondary_IRQ_Cap   : Rose.Capabilities.Capability;

   Command_0_Cap       : Rose.Capabilities.Capability;
   Control_0_Cap       : Rose.Capabilities.Capability;
   Data_0_Cap_8        : Rose.Capabilities.Capability;
   Data_0_Cap_Read_16  : Rose.Capabilities.Capability;
   Data_0_Cap_Write_16 : Rose.Capabilities.Capability;

   Command_1_Cap       : Rose.Capabilities.Capability;
   Control_1_Cap       : Rose.Capabilities.Capability;
   Data_1_Cap_8        : Rose.Capabilities.Capability;
   Data_1_Cap_Read_16  : Rose.Capabilities.Capability;
   Data_1_Cap_Write_16 : Rose.Capabilities.Capability;

   Set_Timeout_Cap     : Rose.Capabilities.Capability;

   Primary_Endpoint   : constant := 16#A3AD_150D#;
   Secondary_Endpoint : constant := 16#5C52_EAF3#;

end ATA;
