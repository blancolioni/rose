with Rose.Server;
with Rose.Words;
with Rose.Capabilities;

package Rose.Interfaces.Storage.Server is

   function Get_Reserve_Storage_Cap return Rose.Capabilities.Capability;
   function Get_Add_Backing_Store_Cap return Rose.Capabilities.Capability;

   type Reserve_Storage_Handler is access
     function
       (Id   : Rose.Objects.Capability_Identifier;
        Size : Rose.Words.Word_64)
     return Rose.Capabilities.Capability;

   type Add_Backing_Store_Handler is access
     procedure
       (Id    : Rose.Objects.Capability_Identifier;
        Store : Rose.Capabilities.Capability);

   procedure Create_Server
     (Server_Context    : in out Rose.Server.Server_Context;
      Reserve_Storage   : in     Reserve_Storage_Handler;
      Add_Backing_Store : in     Add_Backing_Store_Handler;
      Instanced         : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context    : in out Rose.Server.Server_Context;
      Reserve_Storage   : in     Reserve_Storage_Handler;
      Add_Backing_Store : in     Add_Backing_Store_Handler;
      Instanced         : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context    : in out Rose.Server.Server_Context;
      Reserve_Storage   : in     Reserve_Storage_Handler;
      Add_Backing_Store : in     Add_Backing_Store_Handler);

private

end Rose.Interfaces.Storage.Server;
