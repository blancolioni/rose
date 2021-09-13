with Rose.Server;
with Rose.Capabilities;

package Rose.Interfaces.Executable.Server is

   function Get_Launch_Cap return Rose.Capabilities.Capability;

   type Launch_Handler is access
     function
       (Id    : Rose.Objects.Capability_Identifier;
        Image : Rose.Capabilities.Capability;
        Store : Rose.Capabilities.Capability;
        Caps  : Rose.Capabilities.Capability_Array)
     return Rose.Capabilities.Capability;

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler);

private

end Rose.Interfaces.Executable.Server;
