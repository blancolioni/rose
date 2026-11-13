with Rose.Capabilities;

package VGA_Console with Pure is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Screen_Page_Cap     : constant Rose.Capabilities.Capability := 3;
   Cursor_Out_Cap      : constant Rose.Capabilities.Capability := 4;
   Cursor_In_Cap       : constant Rose.Capabilities.Capability := 5;
   Serial_Data_Cap     : constant Rose.Capabilities.Capability := 6;
   Serial_Status_Cap   : constant Rose.Capabilities.Capability := 7;
   Command_Line_Cap    : constant Rose.Capabilities.Capability := 8;
   --  COM2 and the kernel's command line, for a copy of what the console
   --  writes when the command line has serial-console (#904).
   Event_Source_Cap    : constant Rose.Capabilities.Capability := 9;
   --  The keyboard's events, which the installer gives the console after
   --  the caps of vga-console.driver (#454).

end VGA_Console;
