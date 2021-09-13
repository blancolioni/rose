with Rose.Server;
with Rose.Words;
with Rose.Capabilities;

package Rose.Interfaces.Event_Listener.Server is

   function Get_On_Event_Cap return Rose.Capabilities.Capability;

   type On_Event_Handler is access
     procedure
       (Id   : Rose.Objects.Capability_Identifier;
        Code : Rose.Words.Word);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      On_Event       : in     On_Event_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      On_Event       : in     On_Event_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      On_Event       : in     On_Event_Handler);

private

end Rose.Interfaces.Event_Listener.Server;
