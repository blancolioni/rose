with Rose.Server;
with System.Storage_Elements;
with Rose.Words;

package Rose.Interfaces.File.Server is

   type Read_Handler is access
     procedure
       (Id     : in     Rose.Objects.Capability_Identifier;
        Buffer :    out System.Storage_Elements.Storage_Array;
        Last   :    out System.Storage_Elements.Storage_Count);

   type Write_Handler is access
     procedure
       (Id     : Rose.Objects.Capability_Identifier;
        Buffer : System.Storage_Elements.Storage_Array);

   type Seek_Handler is access
     procedure
       (Id                : Rose.Objects.Capability_Identifier;
        Offset_From_Start : Rose.Words.Word_64);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler;
      Write          : in     Write_Handler;
      Seek           : in     Seek_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler;
      Write          : in     Write_Handler;
      Seek           : in     Seek_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler;
      Write          : in     Write_Handler;
      Seek           : in     Seek_Handler);

private

end Rose.Interfaces.File.Server;
