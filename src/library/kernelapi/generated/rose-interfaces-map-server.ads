with Rose.Server;
with Rose.Capabilities;

package Rose.Interfaces.Map.Server is

   type Add_Handler is access
     procedure
       (Id   : Rose.Objects.Capability_Identifier;
        Name : String;
        Cap  : Rose.Capabilities.Capability);

   type Remove_Handler is access
     procedure
       (Id   : Rose.Objects.Capability_Identifier;
        Name : String);

   type Find_Handler is access
     function
       (Id   : Rose.Objects.Capability_Identifier;
        Name : String)
     return Rose.Capabilities.Capability;

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Add            : in     Add_Handler;
      Remove         : in     Remove_Handler;
      Find           : in     Find_Handler;
      Instanced      : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Add            : in     Add_Handler;
      Remove         : in     Remove_Handler;
      Find           : in     Find_Handler;
      Instanced      : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Add            : in     Add_Handler;
      Remove         : in     Remove_Handler;
      Find           : in     Find_Handler);

private

end Rose.Interfaces.Map.Server;
