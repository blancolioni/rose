with Rose.Server;
with Rose.Words;

package Rose.Interfaces.Seekable.Server is

   type Seek_Handler is access
     procedure
       (Id                : Rose.Objects.Capability_Identifier;
        Offset_From_Start : Rose.Words.Word_64);

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Seek           : in     Seek_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Seek           : in     Seek_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Seek           : in     Seek_Handler);

private

end Rose.Interfaces.Seekable.Server;
