with Rose.Capabilities;

package Exec is

   pragma Pure (Exec);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 4;
   Delete_Endpoint_Cap  : constant Rose.Capabilities.Capability := 5;
   Rescind_Endpoint_Cap : constant Rose.Capabilities.Capability := 6;
   Console_Cap          : constant Rose.Capabilities.Capability := 7;

   Create_Process_Cap   : constant Rose.Capabilities.Capability := 8;
   Region_Cap           : constant Rose.Capabilities.Capability := 9;
   Storage_Cap          : constant Rose.Capabilities.Capability := 10;

end Exec;
