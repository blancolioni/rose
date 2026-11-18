with Rose.Capabilities;

--  The boot modules' cap set server (#826). Init gives every boot module
--  after it its caps in one cap set, which the module reads at startup.
--  Once boot is done, init releases the server, and it ends.
--
--  It is the first boot module, before the console, so that the console
--  can have a cap set too (#234), and so it has no console to write to.

package Bootcaps is

   pragma Pure (Bootcaps);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Exit_Cap            : constant Rose.Capabilities.Capability := 2;

end Bootcaps;
