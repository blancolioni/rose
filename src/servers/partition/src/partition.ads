--  Root package for Partition server

with Rose.Capabilities;

package Partition is

   pragma Pure (Partition);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;

   Block_Device_Cap    : constant Rose.Capabilities.Capability := 3;

end Partition;
