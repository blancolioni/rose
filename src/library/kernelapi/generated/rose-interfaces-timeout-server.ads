with Rose.Server;

package Rose.Interfaces.Timeout.Server is

   type On_Timeout_Handler is access
     procedure (Id : Rose.Objects.Capability_Identifier);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      On_Timeout     : in     On_Timeout_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      On_Timeout     : in     On_Timeout_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      On_Timeout     : in     On_Timeout_Handler);

private

end Rose.Interfaces.Timeout.Server;
