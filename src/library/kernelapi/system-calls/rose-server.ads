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

   type Server_Instance is limited private;

   function Has_Instance
     (Instance   : Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier)
      return Boolean;

   procedure Set_Instance_Cap
     (Instance   : in out Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier;
      Cap        : Rose.Capabilities.Capability);

   function Get_Instance_Cap
     (Instance   : Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure Set_Create_Endpoint_Cap
     (Cap      : Rose.Capabilities.Capability);

   function Create_Endpoint
     (Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability;

   procedure Create_Anonymous_Endpoint
     (Endpoint_Id  : Rose.Objects.Endpoint_Id);

private

   Max_Handled_Endpoints  : constant := 20;
   Max_Endpoint_Instances : constant := 80;

   type Instanced_Handler_Record is
      record
         Endpoint   : Rose.Objects.Endpoint_Id;
         Identifier : Rose.Objects.Capability_Identifier;
         Capability : Rose.Capabilities.Capability;
      end record;

   type Instanced_Handler_Array is
     array (1 .. Max_Endpoint_Instances) of Instanced_Handler_Record;

   type Endpoint_Record is
      record
         Endpoint  : Rose.Objects.Endpoint_Id;
         Handler   : Invocation_Handler;
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

   type Server_Instance is limited
      record
         Instances      : Instanced_Handler_Array;
         Instance_Count : Natural := 0;
      end record;

end Rose.Server;
