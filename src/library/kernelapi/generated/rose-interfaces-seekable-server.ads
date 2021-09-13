with Rose.Server;
with Rose.Words;
with Rose.Capabilities;

package Rose.Interfaces.Seekable.Server is

   function Get_Seek_Cap return Rose.Capabilities.Capability;

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
