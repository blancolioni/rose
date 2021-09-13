with Rose.Server;
with Rose.Capabilities;
with System.Storage_Elements;

package Rose.Interfaces.Launch.Server is

   function Get_Launch_Cap return Rose.Capabilities.Capability;

   type Launch_Handler is access
     function
       (Id          : Rose.Objects.Capability_Identifier;
        Caps        : Rose.Capabilities.Capability_Array;
        Environment : System.Storage_Elements.Storage_Array)
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

end Rose.Interfaces.Launch.Server;
