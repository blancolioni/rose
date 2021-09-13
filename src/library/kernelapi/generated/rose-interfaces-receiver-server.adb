with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Receiver.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Send_Cap : Send_Cap_Handler;
   Send_Cap_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Receiver_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Send_Cap (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Send_Cap       : in     Send_Cap_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Send_Cap := Send_Cap;
      Rose.Server.Register_Handler
        (Server_Context,
         Receiver_Interface,
         Handle_Get_Receiver_Interface'Access);
      if not Instanced then
         Send_Cap_Cap := Rose.Server.Create_Endpoint (Send_Cap_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Receiver_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Send_Cap_Endpoint,
         Handle_Send_Cap'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Send_Cap       : in     Send_Cap_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Send_Cap := Send_Cap;
      Rose.Server.Register_Handler
        (Server_Context,
         Receiver_Interface,
         Handle_Get_Receiver_Interface'Access);
      if not Instanced then
         Send_Cap_Cap := Rose.Server.Create_Endpoint (Send_Cap_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Receiver_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Send_Cap_Endpoint,
         Handle_Send_Cap'Access);
   end Create_Server;

   ----------------------
   -- Get_Send_Cap_Cap --
   ----------------------

   function Get_Send_Cap_Cap return Rose.Capabilities.Capability
   is (Send_Cap_Cap);

   -----------------------------------
   -- Handle_Get_Receiver_Interface --
   -----------------------------------

   procedure Handle_Get_Receiver_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Send_Cap_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Send_Cap_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Send_Cap_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Send_Cap_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Send_Cap_Endpoint, Identifier);
      else
         Cap := Send_Cap_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Receiver_Interface;

   ---------------------
   -- Handle_Send_Cap --
   ---------------------

   procedure Handle_Send_Cap (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Cap : constant Rose.Capabilities.Capability := Parameters.Caps (0);
   begin
      Local_Send_Cap (Parameters.Identifier, Cap);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Send_Cap;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Send_Cap       : in     Send_Cap_Handler)
   is
   begin
      Local_Send_Cap := Send_Cap;
      Send_Cap_Cap := Rose.Server.Create_Endpoint (Send_Cap_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Receiver_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Receiver_Interface,
         Handle_Get_Receiver_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Send_Cap_Endpoint,
         Handle_Send_Cap'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Receiver.Server;
