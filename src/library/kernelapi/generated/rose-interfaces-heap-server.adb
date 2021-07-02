with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;
with Rose.Capabilities;

package body Rose.Interfaces.Heap.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Current_Bound : Current_Bound_Handler;
   Current_Bound_Cap : Rose.Capabilities.Capability := 0;
   Local_Request_New_Bound : Request_New_Bound_Handler;
   Request_New_Bound_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Heap_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Current_Bound (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Request_New_Bound (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context    : in out Rose.Server.Server_Context;
      Current_Bound     : in     Current_Bound_Handler;
      Request_New_Bound : in     Request_New_Bound_Handler;
      Instanced         : in     Boolean := False)
   is
   begin
      Local_Current_Bound := Current_Bound;
      Local_Request_New_Bound := Request_New_Bound;
      Rose.Server.Register_Handler
        (Server_Context,
         Heap_Interface,
         Handle_Get_Heap_Interface'Access);
      if not Instanced then
         Current_Bound_Cap := Rose.Server.Create_Endpoint
            (Current_Bound_Endpoint);
         Request_New_Bound_Cap := Rose.Server.Create_Endpoint
            (Request_New_Bound_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Heap_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Current_Bound_Endpoint,
         Handle_Current_Bound'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Request_New_Bound_Endpoint,
         Handle_Request_New_Bound'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context    : in out Rose.Server.Server_Context;
      Current_Bound     : in     Current_Bound_Handler;
      Request_New_Bound : in     Request_New_Bound_Handler;
      Instanced         : in     Boolean := False)
   is
   begin
      Local_Current_Bound := Current_Bound;
      Local_Request_New_Bound := Request_New_Bound;
      Rose.Server.Register_Handler
        (Server_Context,
         Heap_Interface,
         Handle_Get_Heap_Interface'Access);
      if not Instanced then
         Current_Bound_Cap := Rose.Server.Create_Endpoint
            (Current_Bound_Endpoint);
         Request_New_Bound_Cap := Rose.Server.Create_Endpoint
            (Request_New_Bound_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Heap_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Current_Bound_Endpoint,
         Handle_Current_Bound'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Request_New_Bound_Endpoint,
         Handle_Request_New_Bound'Access);
   end Create_Server;

   --------------------------
   -- Handle_Current_Bound --
   --------------------------

   procedure Handle_Current_Bound (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Words.Word := Local_Current_Bound
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters, Result);
   end Handle_Current_Bound;

   -------------------------------
   -- Handle_Get_Heap_Interface --
   -------------------------------

   procedure Handle_Get_Heap_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Current_Bound_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Current_Bound_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Current_Bound_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Current_Bound_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Current_Bound_Endpoint, Identifier);
      else
         Cap := Current_Bound_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Request_New_Bound_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Request_New_Bound_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Request_New_Bound_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Request_New_Bound_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Request_New_Bound_Endpoint, Identifier);
      else
         Cap := Request_New_Bound_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Heap_Interface;

   ------------------------------
   -- Handle_Request_New_Bound --
   ------------------------------

   procedure Handle_Request_New_Bound (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      New_Bound : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 0);
   begin
      Local_Request_New_Bound (Parameters.Identifier, New_Bound);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Request_New_Bound;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context    : in out Rose.Server.Server_Context;
      Current_Bound     : in     Current_Bound_Handler;
      Request_New_Bound : in     Request_New_Bound_Handler)
   is
   begin
      Local_Current_Bound := Current_Bound;
      Local_Request_New_Bound := Request_New_Bound;
      Current_Bound_Cap := Rose.Server.Create_Endpoint
         (Current_Bound_Endpoint);
      Request_New_Bound_Cap := Rose.Server.Create_Endpoint
         (Request_New_Bound_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Heap_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Heap_Interface,
         Handle_Get_Heap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Current_Bound_Endpoint,
         Handle_Current_Bound'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Request_New_Bound_Endpoint,
         Handle_Request_New_Bound'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Heap.Server;
