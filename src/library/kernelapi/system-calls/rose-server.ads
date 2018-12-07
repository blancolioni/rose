with Rose.Capabilities;
with Rose.Invocation;
with Rose.Objects;

package Rose.Server is

   type Invocation_Handler is access
     procedure (Invocation : in out Rose.Invocation.Invocation_Record);

   type Server_Context is limited private;

   procedure Register_Handler
     (Context  : in out Server_Context;
      Endpoint : Rose.Objects.Endpoint_Id;
      Handler  : Invocation_Handler);

   procedure Receive_Message
     (Context : in out Server_Context);
   --  wait for a single message on one of the context endpoints

   procedure Start_Server
     (Context : in out Server_Context);
   --  receive messages until the server is killed

private

   Max_Handled_Endpoints : constant := 20;

   type Endpoint_Record is
      record
         Endpoint : Rose.Objects.Endpoint_Id;
         Handler  : Invocation_Handler;
      end record;

   type Endpoint_Array is
     array (1 .. Max_Handled_Endpoints) of Endpoint_Record;

   type Server_Context is limited
      record
         Receive_Cap    : Rose.Capabilities.Capability :=
                            Rose.Capabilities.Null_Capability;
         Endpoint_Count : Natural := 0;
         Endpoints      : Endpoint_Array;
      end record;

end Rose.Server;
