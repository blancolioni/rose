with Rose.Server;
with Rose.Capabilities;

package Rose.Interfaces.Ata.Server is

   function Get_Get_Device_Cap return Rose.Capabilities.Capability;

   type Get_Device_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Capabilities.Capability;

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Device     : in     Get_Device_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Device     : in     Get_Device_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Device     : in     Get_Device_Handler);

private

end Rose.Interfaces.Ata.Server;
