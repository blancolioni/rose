with Rose.Server;
with Rose.Capabilities;

package Rose.Interfaces.Event_Source.Server is

   type Add_Listener_Handler is access
     procedure
       (Id       : Rose.Objects.Capability_Identifier;
        Listener : Rose.Capabilities.Capability);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Add_Listener   : in     Add_Listener_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Add_Listener   : in     Add_Listener_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Add_Listener   : in     Add_Listener_Handler);

private

end Rose.Interfaces.Event_Source.Server;
