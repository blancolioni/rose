with Rose.Capabilities;

package ATA is

   pragma Pure (ATA);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;
   PCI_Cap             : constant Rose.Capabilities.Capability := 3;

   Command_0_Cap       : constant Rose.Capabilities.Capability := 4;
   Control_0_Cap       : constant Rose.Capabilities.Capability := 5;
   Data_0_Cap_8        : constant Rose.Capabilities.Capability := 6;
   Data_0_Cap_16       : constant Rose.Capabilities.Capability := 7;
   Command_1_Cap       : constant Rose.Capabilities.Capability := 8;
   Control_1_Cap       : constant Rose.Capabilities.Capability := 9;
   Data_1_Cap_8        : constant Rose.Capabilities.Capability := 10;
   Data_1_Cap_16       : constant Rose.Capabilities.Capability := 11;

end ATA;
