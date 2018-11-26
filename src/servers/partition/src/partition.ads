--  Root package for Partition server

with Rose.Capabilities;

package Partition is

   pragma Pure (Partition);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;

   Device_Parameters_Cap : constant Rose.Capabilities.Capability := 3;
   Device_Read_Cap       : constant Rose.Capabilities.Capability := 4;
   Device_Write_Cap      : constant Rose.Capabilities.Capability := 5;

end Partition;
