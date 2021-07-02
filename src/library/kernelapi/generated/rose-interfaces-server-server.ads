with Rose.Server;
with Rose.Capabilities;

package Rose.Interfaces.Server.Server is

   type Published_Interface_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Capabilities.Capability;

   procedure Create_Server
     (Server_Context      : in out Rose.Server.Server_Context;
      Published_Interface : in     Published_Interface_Handler;
      Instanced           : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context      : in out Rose.Server.Server_Context;
      Published_Interface : in     Published_Interface_Handler;
      Instanced           : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context      : in out Rose.Server.Server_Context;
      Published_Interface : in     Published_Interface_Handler);

private

end Rose.Interfaces.Server.Server;
