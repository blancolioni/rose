with Rose.Capabilities;

package Scan is

   pragma Pure (Scan);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;
   Block_Device_Cap    : constant Rose.Capabilities.Capability := 3;

end Scan;
