with Rose.Server;
with Rose.Capabilities;
with System.Storage_Elements;

package Rose.Interfaces.Exec.Server is

   type Launch_Handler is access
     function
       (Id          : Rose.Objects.Capability_Identifier;
        Caps        : Rose.Capabilities.Capability_Array;
        Environment : System.Storage_Elements.Storage_Array)
     return Rose.Capabilities.Capability;

   type Install_Handler is access
     function
       (Id           : Rose.Objects.Capability_Identifier;
        Executable   : Rose.Capabilities.Capability;
        Install_Caps : Rose.Capabilities.Capability_Array)
     return Rose.Capabilities.Capability;

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler;
      Install        : in     Install_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler;
      Install        : in     Install_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler;
      Install        : in     Install_Handler);

private

end Rose.Interfaces.Exec.Server;
