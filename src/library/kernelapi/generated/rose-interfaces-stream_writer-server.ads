with Rose.Server;
with System.Storage_Elements;
with Rose.Capabilities;

package Rose.Interfaces.Stream_Writer.Server is

   function Get_Write_Cap return Rose.Capabilities.Capability;

   type Write_Handler is access
     procedure
       (Id     : Rose.Objects.Capability_Identifier;
        Buffer : System.Storage_Elements.Storage_Array);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Write          : in     Write_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Write          : in     Write_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Write          : in     Write_Handler);

private

end Rose.Interfaces.Stream_Writer.Server;
