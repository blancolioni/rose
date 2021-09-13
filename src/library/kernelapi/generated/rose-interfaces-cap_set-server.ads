with Rose.Server;
with Rose.Capabilities;

package Rose.Interfaces.Cap_Set.Server is

   function Get_Append_Cap return Rose.Capabilities.Capability;
   function Get_Get_Cap_Cap return Rose.Capabilities.Capability;
   function Get_Length_Cap return Rose.Capabilities.Capability;

   type Append_Handler is access
     procedure
       (Id   : Rose.Objects.Capability_Identifier;
        Caps : Rose.Capabilities.Capability_Array);

   type Get_Cap_Handler is access
     function
       (Id    : Rose.Objects.Capability_Identifier;
        Index : Positive)
     return Rose.Capabilities.Capability;

   type Length_Handler is access
     function (Id : Rose.Objects.Capability_Identifier) return Natural;

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Append         : in     Append_Handler;
      Get_Cap        : in     Get_Cap_Handler;
      Length         : in     Length_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Append         : in     Append_Handler;
      Get_Cap        : in     Get_Cap_Handler;
      Length         : in     Length_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Append         : in     Append_Handler;
      Get_Cap        : in     Get_Cap_Handler;
      Length         : in     Length_Handler);

private

end Rose.Interfaces.Cap_Set.Server;
