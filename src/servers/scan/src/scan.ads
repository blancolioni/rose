with Rose.Capabilities;

package Scan is

   pragma Pure (Scan);

   Create_Endpoint_Cap           : constant Rose.Capabilities.Capability := 1;
   Console_Cap                   : constant Rose.Capabilities.Capability := 2;
   Block_Device_Parameters_Cap   : constant Rose.Capabilities.Capability := 3;
   Block_Device_Read_Cap         : constant Rose.Capabilities.Capability := 4;
   Block_Device_Write_Cap        : constant Rose.Capabilities.Capability := 5;

end Scan;
