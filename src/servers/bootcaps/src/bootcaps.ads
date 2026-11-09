with Rose.Capabilities;

--  The boot modules' cap set server (#826). Init gives every boot module
--  after it its caps in one cap set, which the module reads at startup.
--  Once boot is done, init releases the server, and it ends.

package Bootcaps is

   pragma Pure (Bootcaps);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;
   Exit_Cap            : constant Rose.Capabilities.Capability := 3;

end Bootcaps;
