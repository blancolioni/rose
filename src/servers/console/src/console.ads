--  Root package for Console server
with Rose.Capabilities;

package Console is

   pragma Pure (Console);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Memory_Cap  : constant Rose.Capabilities.Capability := 2;
   Console_Cursor_Cap  : constant Rose.Capabilities.Capability := 3;
   Serial_Data_Cap     : constant Rose.Capabilities.Capability := 4;
   Serial_Status_Cap   : constant Rose.Capabilities.Capability := 5;
   Framebuffer_Tag_Cap : constant Rose.Capabilities.Capability := 6;
   Command_Line_Cap    : constant Rose.Capabilities.Capability := 7;
   Mirror_Data_Cap     : constant Rose.Capabilities.Capability := 8;
   Mirror_Status_Cap   : constant Rose.Capabilities.Capability := 9;
   --  The kernel's command line, which may ask for a copy of the console
   --  on COM2 (serial-console, #904), and COM2's data and line status.

end Console;
