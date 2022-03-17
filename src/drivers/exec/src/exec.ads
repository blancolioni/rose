with Rose.Capabilities;

package Exec is

   pragma Pure (Exec);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 2;
   Delete_Endpoint_Cap  : constant Rose.Capabilities.Capability := 3;
   Rescind_Endpoint_Cap : constant Rose.Capabilities.Capability := 4;
   Console_Cap          : constant Rose.Capabilities.Capability := 5;

   Create_Process_Cap   : constant Rose.Capabilities.Capability := 6;
   Region_Cap           : constant Rose.Capabilities.Capability := 7;
   Storage_Cap          : constant Rose.Capabilities.Capability := 8;

end Exec;
