with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Event_Source.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Add_Listener : Add_Listener_Handler;
   Add_Listener_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Event_Source_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Add_Listener (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Add_Listener   : in     Add_Listener_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Add_Listener := Add_Listener;
      Rose.Server.Register_Handler
        (Server_Context,
         Event_Source_Interface,
         Handle_Get_Event_Source_Interface'Access);
      if not Instanced then
         Add_Listener_Cap := Rose.Server.Create_Endpoint
            (Add_Listener_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Event_Source_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Listener_Endpoint,
         Handle_Add_Listener'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Add_Listener   : in     Add_Listener_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Add_Listener := Add_Listener;
      Rose.Server.Register_Handler
        (Server_Context,
         Event_Source_Interface,
         Handle_Get_Event_Source_Interface'Access);
      if not Instanced then
         Add_Listener_Cap := Rose.Server.Create_Endpoint
            (Add_Listener_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Event_Source_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Listener_Endpoint,
         Handle_Add_Listener'Access);
   end Create_Server;

   -------------------------
   -- Handle_Add_Listener --
   -------------------------

   procedure Handle_Add_Listener (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Listener : constant Rose.Capabilities.Capability := Parameters.Caps
         (0);
   begin
      Local_Add_Listener (Parameters.Identifier, Listener);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Add_Listener;

   ---------------------------------------
   -- Handle_Get_Event_Source_Interface --
   ---------------------------------------

   procedure Handle_Get_Event_Source_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Add_Listener_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Add_Listener_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Add_Listener_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Add_Listener_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Add_Listener_Endpoint, Identifier);
      else
         Cap := Add_Listener_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Event_Source_Interface;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Add_Listener   : in     Add_Listener_Handler)
   is
   begin
      Local_Add_Listener := Add_Listener;
      Add_Listener_Cap := Rose.Server.Create_Endpoint
         (Add_Listener_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Event_Source_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Event_Source_Interface,
         Handle_Get_Event_Source_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Listener_Endpoint,
         Handle_Add_Listener'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Event_Source.Server;
