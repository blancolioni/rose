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

end Console;
