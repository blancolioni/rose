--  Root package for Console server
with Rose.Capabilities;

package Console is

   pragma Pure (Console);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Memory_Cap  : constant Rose.Capabilities.Capability := 2;
   Console_Cursor_Cap  : constant Rose.Capabilities.Capability := 3;

end Console;
