with Rose.Server;
with Rose.Words;

package Rose.Interfaces.Heap.Server is

   type Current_Bound_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Words.Word;

   type Request_New_Bound_Handler is access
     procedure
       (Id        : Rose.Objects.Capability_Identifier;
        New_Bound : Rose.Words.Word);

   procedure Create_Server
     (Server_Context    : in out Rose.Server.Server_Context;
      Current_Bound     : in     Current_Bound_Handler;
      Request_New_Bound : in     Request_New_Bound_Handler;
      Instanced         : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context    : in out Rose.Server.Server_Context;
      Current_Bound     : in     Current_Bound_Handler;
      Request_New_Bound : in     Request_New_Bound_Handler;
      Instanced         : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context    : in out Rose.Server.Server_Context;
      Current_Bound     : in     Current_Bound_Handler;
      Request_New_Bound : in     Request_New_Bound_Handler);

private

end Rose.Interfaces.Heap.Server;
