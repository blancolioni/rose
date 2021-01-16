with Rose.Capabilities;

package Exec is

   pragma Pure (Exec);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 3;
   Delete_Endpoint_Cap  : constant Rose.Capabilities.Capability := 4;
   Rescind_Endpoint_Cap : constant Rose.Capabilities.Capability := 5;
   Console_Cap          : constant Rose.Capabilities.Capability := 6;

   Create_Process_Cap   : constant Rose.Capabilities.Capability := 7;
   Region_Cap           : constant Rose.Capabilities.Capability := 8;
   Storage_Cap          : constant Rose.Capabilities.Capability := 9;

end Exec;
