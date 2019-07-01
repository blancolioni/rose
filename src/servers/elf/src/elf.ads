--  Root package for Store server

with Rose.Capabilities;

package Elf is

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 1;
   Take_Next_Cap        : constant Rose.Capabilities.Capability := 2;

   Delete_Endpoint_Cap  : Rose.Capabilities.Capability;
   Rescind_Endpoint_Cap : Rose.Capabilities.Capability;
   Console_Cap          : Rose.Capabilities.Capability;
   Memory_Cap           : Rose.Capabilities.Capability;
   Create_Process_Cap   : Rose.Capabilities.Capability;

end Elf;
