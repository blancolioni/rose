--  Root package for Console server
with Rose.Capabilities;

package Console is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;
   --  The console's caps are in a cap set, which it reads at startup
   --  (Console.Calls.Get_Caps), like every boot module after bootcaps
   --  (#234).

   Console_Memory_Cap  : Rose.Capabilities.Capability := 0;
   Console_Cursor_Cap  : Rose.Capabilities.Capability := 0;
   Serial_Data_Cap     : Rose.Capabilities.Capability := 0;
   Serial_Status_Cap   : Rose.Capabilities.Capability := 0;
   Framebuffer_Tag_Cap : Rose.Capabilities.Capability := 0;
   Command_Line_Cap    : Rose.Capabilities.Capability := 0;
   Mirror_Data_Cap     : Rose.Capabilities.Capability := 0;
   Mirror_Status_Cap   : Rose.Capabilities.Capability := 0;
   --  The kernel's command line, which may ask for a copy of the console
   --  on COM2 (serial-console, #904), and COM2's data and line status.

end Console;
