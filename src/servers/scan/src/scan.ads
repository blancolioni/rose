with Rose.Capabilities;

package Scan is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;

   Console_Cap         : Rose.Capabilities.Capability;
   Block_Device_Cap    : Rose.Capabilities.Capability;
   Exit_Cap            : Rose.Capabilities.Capability;
   --  Scan exits when init releases its Partitions (#463).

end Scan;
