with Rose.Server;
with System.Storage_Elements;

package Rose.Interfaces.Terminal.Server is

   type Read_Handler is access
     procedure
       (Id     : in     Rose.Objects.Capability_Identifier;
        Buffer :    out System.Storage_Elements.Storage_Array;
        Last   :    out System.Storage_Elements.Storage_Count);

   type Write_Handler is access
     procedure
       (Id     : Rose.Objects.Capability_Identifier;
        Buffer : System.Storage_Elements.Storage_Array);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler;
      Write          : in     Write_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler;
      Write          : in     Write_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler;
      Write          : in     Write_Handler);

private

end Rose.Interfaces.Terminal.Server;
