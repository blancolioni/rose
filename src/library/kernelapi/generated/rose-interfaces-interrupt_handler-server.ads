with Rose.Server;
with Rose.Words;

package Rose.Interfaces.Interrupt_Handler.Server is

   type Handle_Interrupt_Handler is access
     procedure
       (Id   : Rose.Objects.Capability_Identifier;
        Code : Rose.Words.Word);

   procedure Create_Server
     (Server_Context   : in out Rose.Server.Server_Context;
      Handle_Interrupt : in     Handle_Interrupt_Handler;
      Instanced        : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context   : in out Rose.Server.Server_Context;
      Handle_Interrupt : in     Handle_Interrupt_Handler;
      Instanced        : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context   : in out Rose.Server.Server_Context;
      Handle_Interrupt : in     Handle_Interrupt_Handler);

private

end Rose.Interfaces.Interrupt_Handler.Server;
