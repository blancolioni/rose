with Rose.Server;
with Rose.Capabilities;

package Rose.Interfaces.File_System.Server is

   function Get_Root_Directory_Cap return Rose.Capabilities.Capability;

   type Root_Directory_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Capabilities.Capability;

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Root_Directory : in     Root_Directory_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Root_Directory : in     Root_Directory_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Root_Directory : in     Root_Directory_Handler);

private

end Rose.Interfaces.File_System.Server;
