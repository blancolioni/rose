with Rose.Capabilities;

package Launch is

   pragma Pure (Launch);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Delete_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Console_Cap         : constant Rose.Capabilities.Capability := 3;

   Create_Process_Cap  : constant Rose.Capabilities.Capability := 4;

end Launch;
