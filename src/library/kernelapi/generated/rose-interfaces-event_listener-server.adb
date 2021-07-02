with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;
with Rose.Capabilities;

package body Rose.Interfaces.Event_Listener.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_On_Event : On_Event_Handler;
   On_Event_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Event_Listener_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_On_Event (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      On_Event       : in     On_Event_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_On_Event := On_Event;
      Rose.Server.Register_Handler
        (Server_Context,
         Event_Listener_Interface,
         Handle_Get_Event_Listener_Interface'Access);
      if not Instanced then
         On_Event_Cap := Rose.Server.Create_Endpoint (On_Event_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Event_Listener_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         On_Event_Endpoint,
         Handle_On_Event'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      On_Event       : in     On_Event_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_On_Event := On_Event;
      Rose.Server.Register_Handler
        (Server_Context,
         Event_Listener_Interface,
         Handle_Get_Event_Listener_Interface'Access);
      if not Instanced then
         On_Event_Cap := Rose.Server.Create_Endpoint (On_Event_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Event_Listener_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         On_Event_Endpoint,
         Handle_On_Event'Access);
   end Create_Server;

   -----------------------------------------
   -- Handle_Get_Event_Listener_Interface --
   -----------------------------------------

   procedure Handle_Get_Event_Listener_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if On_Event_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, On_Event_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               On_Event_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (On_Event_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            On_Event_Endpoint, Identifier);
      else
         Cap := On_Event_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Event_Listener_Interface;

   ---------------------
   -- Handle_On_Event --
   ---------------------

   procedure Handle_On_Event (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Code : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 0);
   begin
      Local_On_Event (Parameters.Identifier, Code);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_On_Event;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      On_Event       : in     On_Event_Handler)
   is
   begin
      Local_On_Event := On_Event;
      On_Event_Cap := Rose.Server.Create_Endpoint (On_Event_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint
         (Event_Listener_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Event_Listener_Interface,
         Handle_Get_Event_Listener_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         On_Event_Endpoint,
         Handle_On_Event'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Event_Listener.Server;
