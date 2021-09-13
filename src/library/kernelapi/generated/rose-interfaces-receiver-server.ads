with Rose.Server;
with Rose.Capabilities;

package Rose.Interfaces.Receiver.Server is

   function Get_Send_Cap_Cap return Rose.Capabilities.Capability;

   type Send_Cap_Handler is access
     procedure
       (Id  : Rose.Objects.Capability_Identifier;
        Cap : Rose.Capabilities.Capability);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Send_Cap       : in     Send_Cap_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Send_Cap       : in     Send_Cap_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Send_Cap       : in     Send_Cap_Handler);

private

end Rose.Interfaces.Receiver.Server;
