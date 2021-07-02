with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;
with Rose.Capabilities;

package body Rose.Interfaces.Timeout.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_On_Timeout : On_Timeout_Handler;
   On_Timeout_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Timeout_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_On_Timeout (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      On_Timeout     : in     On_Timeout_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_On_Timeout := On_Timeout;
      Rose.Server.Register_Handler
        (Server_Context,
         Timeout_Interface,
         Handle_Get_Timeout_Interface'Access);
      if not Instanced then
         On_Timeout_Cap := Rose.Server.Create_Endpoint (On_Timeout_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Timeout_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         On_Timeout_Endpoint,
         Handle_On_Timeout'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      On_Timeout     : in     On_Timeout_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_On_Timeout := On_Timeout;
      Rose.Server.Register_Handler
        (Server_Context,
         Timeout_Interface,
         Handle_Get_Timeout_Interface'Access);
      if not Instanced then
         On_Timeout_Cap := Rose.Server.Create_Endpoint (On_Timeout_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Timeout_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         On_Timeout_Endpoint,
         Handle_On_Timeout'Access);
   end Create_Server;

   ----------------------------------
   -- Handle_Get_Timeout_Interface --
   ----------------------------------

   procedure Handle_Get_Timeout_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if On_Timeout_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            On_Timeout_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               On_Timeout_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (On_Timeout_Endpoint,
               Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            On_Timeout_Endpoint, Identifier);
      else
         Cap := On_Timeout_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Timeout_Interface;

   -----------------------
   -- Handle_On_Timeout --
   -----------------------

   procedure Handle_On_Timeout (Parameters :
      in out Rose.Invocation.Invocation_Record) is
   begin
      Local_On_Timeout (Parameters.Identifier);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_On_Timeout;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      On_Timeout     : in     On_Timeout_Handler)
   is
   begin
      Local_On_Timeout := On_Timeout;
      On_Timeout_Cap := Rose.Server.Create_Endpoint (On_Timeout_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Timeout_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Timeout_Interface,
         Handle_Get_Timeout_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         On_Timeout_Endpoint,
         Handle_On_Timeout'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Timeout.Server;
