with Rose.Capabilities;

package Caps with Pure is

   --  The userland cap set server, for persistent processes (#825). The
   --  boot modules have their own, bootcaps, which ends after boot (#826).

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 2;
   Delete_Endpoint_Cap  : constant Rose.Capabilities.Capability := 3;
   Rescind_Endpoint_Cap : constant Rose.Capabilities.Capability := 4;
   --  The caps of caps.driver.

   Console_Writer_Cap   : constant Rose.Capabilities.Capability := 5;
   --  The persistent console's Writer interface, which the installer
   --  gives after the driver's own caps (Requires_Console).

end Caps;
