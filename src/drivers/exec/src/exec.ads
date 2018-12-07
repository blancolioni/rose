with Rose.Capabilities;

package Exec is

   pragma Pure (Exec);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 1;
   Delete_Endpoint_Cap  : constant Rose.Capabilities.Capability := 2;
   Rescind_Endpoint_Cap : constant Rose.Capabilities.Capability := 3;
   Console_Cap          : constant Rose.Capabilities.Capability := 4;

   Create_Process_Cap   : constant Rose.Capabilities.Capability := 5;
   Storage_Cap          : constant Rose.Capabilities.Capability := 6;

end Exec;
