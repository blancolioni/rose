with Rose.Server;
with Rose.Words;
with Rose.Capabilities;

package Rose.Interfaces.Timer.Server is

   type Set_Timer_Handler is access
     function
       (Id           : Rose.Objects.Capability_Identifier;
        Milliseconds : Rose.Words.Word;
        Cap          : Rose.Capabilities.Capability)
     return Rose.Capabilities.Capability;

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Set_Timer      : in     Set_Timer_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Set_Timer      : in     Set_Timer_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Set_Timer      : in     Set_Timer_Handler);

private

end Rose.Interfaces.Timer.Server;
