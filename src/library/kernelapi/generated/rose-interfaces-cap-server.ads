with Rose.Server;
with Rose.Objects;

package Rose.Interfaces.Cap.Server is

   type Destroy_Handler is access
     procedure (Id : Rose.Objects.Capability_Identifier);

   type Get_Object_Id_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Objects.Object_Id;

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler);

private

end Rose.Interfaces.Cap.Server;
