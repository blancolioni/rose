with Rose.Capabilities;

package Surface_Console with Pure is

   --  The persistent console on a surface (ADR 0005, ADR 0022, #653):
   --  the screen's surface when the boot loader gives a framebuffer, in
   --  place of vga-console on the VGA text page.

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Create_Signal_Cap   : constant Rose.Capabilities.Capability := 3;
   Serial_Data_Cap     : constant Rose.Capabilities.Capability := 4;
   Serial_Status_Cap   : constant Rose.Capabilities.Capability := 5;
   Command_Line_Cap    : constant Rose.Capabilities.Capability := 6;
   --  COM2 and the kernel's command line, for a copy of what the console
   --  writes when the command line has serial-console (#904). The menu's
   --  serial-console entries are in text mode, so this console only
   --  copies if one is added on a framebuffer.
   Surface_Cap         : constant Rose.Capabilities.Capability := 7;
   --  The screen driver's surface, which the installer gives after the
   --  caps of surface-console.driver.

end Surface_Console;
