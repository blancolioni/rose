with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

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

   type Owner_Function is access
     function (Identifier : Rose.Objects.Capability_Identifier)
               return Boolean;

   procedure Register_Handler
     (Context  : in out Server_Context;
      Endpoint : Rose.Objects.Endpoint_Id;
      Handler  : Invocation_Handler;
      Owns     : Owner_Function);
   --  As Register_Handler, for a handler that serves only the objects
   --  for which Owns is true (idle#84). Several handlers can share an
   --  endpoint, as the server packages of two interfaces with a common
   --  ancestor do: a call goes to the first whose Owns is true for its
   --  identifier. A handler registered without Owns serves every
   --  identifier.

   function New_Identifier return Rose.Objects.Capability_Identifier;
   --  An identifier for a new object, unique in the process among the
   --  identifiers in use, so that the objects of every server package in
   --  the process can be told apart (idle#84). It is never 0, the
   --  identifier of an endpoint that is not instanced. A freed identifier
   --  is given out again only after every other one has been. Raises
   --  Constraint_Error if all are in use.

   procedure Free_Identifier
     (Identifier : Rose.Objects.Capability_Identifier);
   --  Identifier can be given out again. Call it only once every cap
   --  with Identifier has been rescinded, so that none can reach the next
   --  object to have it.

   procedure Receive_Message
     (Context : in out Server_Context);
   --  wait for a single message on one of the context endpoints

   procedure Start_Server
     (Context : in out Server_Context);
   --  receive messages until the server is killed

   procedure Block_Current_Request
     (Context : in out Server_Context);

   procedure Unblock_Endpoint
     (Context  : in out Server_Context;
      Endpoint : Rose.Objects.Endpoint_Id);

   --  Signals (ADR 0006, #708). A process makes signal caps for its own
   --  bits with Rose.System_Calls.Create_Signal_Cap, and gives them to
   --  whoever is to signal it. When it next receives, the kernel sends
   --  every bit that is set, and Rose.Server calls the handler for each.

   subtype Signal_Bit is Natural range 8 .. 31;

   type Signal_Handler is access procedure;

   procedure Set_Signal_Handler
     (Context : in out Server_Context;
      Bit     : Signal_Bit;
      Handler : Signal_Handler);
   --  Call Handler when Bit is signalled. The first handler attaches the
   --  Signalled interface to Context, so a process that sets none sees
   --  no change. Bits with no handler are ignored.

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

   procedure Remove_Instance
     (Instance   : in out Server_Instance;
      Identifier : Rose.Objects.Capability_Identifier);
   --  Delete the caps of the endpoints for Identifier in Instance, and
   --  forget them: the object Identifier is gone.

   function Get_Instance_Cap
     (Instance   : Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Create_Endpoint
     (Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability;

   procedure Create_Anonymous_Endpoint
     (Endpoint_Id  : Rose.Objects.Endpoint_Id);

   procedure Publish_Interface
     (Interface_Cap : Rose.Capabilities.Capability);
   --  Publish Interface_Cap through the Process interface of the current
   --  process, which must have been started through exec
   --  (see Rose.Capabilities.Standard).

private

   Max_Handled_Endpoints  : constant := 64;
   --  As many as the kernel registers for a process
   --  (Rose.Kernel.Processes.Max_Registered_Endpoints).

   Max_Handlers : constant := 2 * Max_Handled_Endpoints;
   --  Handlers that share an endpoint each have a record, so there can be
   --  more records than endpoints.

   --  A userland process has a heap, so the table of an instanced server
   --  has no fixed size: it holds the caps of each object, found by the
   --  object's identifier, and grows with the objects the server serves.

   type Instanced_Handler_Record is
      record
         Endpoint   : Rose.Objects.Endpoint_Id;
         Capability : Rose.Capabilities.Capability;
      end record;

   package Handler_Vectors is
     new Ada.Containers.Vectors (Positive, Instanced_Handler_Record);

   function Hash_Identifier
     (Identifier : Rose.Objects.Capability_Identifier)
      return Ada.Containers.Hash_Type;

   package Instance_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Rose.Objects.Capability_Identifier,
        Element_Type    => Handler_Vectors.Vector,
        Hash            => Hash_Identifier,
        Equivalent_Keys => Rose.Objects."=",
        "="             => Handler_Vectors."=");

   type Endpoint_Record is
      record
         Endpoint  : Rose.Objects.Endpoint_Id;
         Handler   : Invocation_Handler;
         Owns      : Owner_Function;
         Blocked   : Boolean;
         Params    : Rose.Invocation.Invocation_Record;
      end record;

   type Endpoint_Array is
     array (1 .. Max_Handlers) of Endpoint_Record;

   type Server_Context is limited
      record
         Receive_Cap    : Rose.Capabilities.Capability :=
                            Rose.Capabilities.Null_Capability;
         Endpoint_Count : Natural := 0;
         Endpoints      : Endpoint_Array;
         Block_Reply    : Boolean := False;
         Has_Signals    : Boolean := False;
      end record;

   type Server_Instance is limited
      record
         Instances : Instance_Maps.Map;
      end record;

end Rose.Server;
