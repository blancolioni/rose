with Rose.Capabilities;

package Surface_Console with Pure is

   --  The persistent console on a surface (ADR 0005, ADR 0022, #653):
   --  the screen's surface when the boot loader gives a framebuffer, in
   --  place of vga-console on the VGA text page.

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Create_Signal_Cap   : constant Rose.Capabilities.Capability := 3;
   Surface_Cap         : constant Rose.Capabilities.Capability := 4;
   --  The screen driver's surface, which the installer gives after the
   --  caps of surface-console.driver.

end Surface_Console;
