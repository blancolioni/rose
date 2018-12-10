--  Root package for Store server

with Rose.Capabilities;

package Elf is

   pragma Pure (Elf);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 1;
   Delete_Endpoint_Cap  : constant Rose.Capabilities.Capability := 2;
   Rescind_Endpoint_Cap : constant Rose.Capabilities.Capability := 3;
   Console_Cap          : constant Rose.Capabilities.Capability := 4;
   Memory_Cap           : constant Rose.Capabilities.Capability := 5;
   Create_Process_Cap   : constant Rose.Capabilities.Capability := 6;

end Elf;
