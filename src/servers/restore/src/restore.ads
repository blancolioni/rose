with Rose.Capabilities;

package Restore is

   pragma Pure (Restore);

   Create_Endpoint_Cap           : constant Rose.Capabilities.Capability := 1;
   Console_Cap                   : constant Rose.Capabilities.Capability := 2;
   Block_Device_Parameters_Cap   : constant Rose.Capabilities.Capability := 3;
   Block_Device_Read_Cap         : constant Rose.Capabilities.Capability := 4;
   Block_Device_Write_Cap        : constant Rose.Capabilities.Capability := 5;
   Install_File_System           : constant Rose.Capabilities.Capability := 6;

end Restore;
