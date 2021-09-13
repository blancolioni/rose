with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Timer.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Set_Timer : Set_Timer_Handler;
   Set_Timer_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Timer_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Set_Timer (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Set_Timer      : in     Set_Timer_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Set_Timer := Set_Timer;
      Rose.Server.Register_Handler
        (Server_Context,
         Timer_Interface,
         Handle_Get_Timer_Interface'Access);
      if not Instanced then
         Set_Timer_Cap := Rose.Server.Create_Endpoint (Set_Timer_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Timer_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Set_Timer_Endpoint,
         Handle_Set_Timer'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Set_Timer      : in     Set_Timer_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Set_Timer := Set_Timer;
      Rose.Server.Register_Handler
        (Server_Context,
         Timer_Interface,
         Handle_Get_Timer_Interface'Access);
      if not Instanced then
         Set_Timer_Cap := Rose.Server.Create_Endpoint (Set_Timer_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Timer_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Set_Timer_Endpoint,
         Handle_Set_Timer'Access);
   end Create_Server;

   -----------------------
   -- Get_Set_Timer_Cap --
   -----------------------

   function Get_Set_Timer_Cap return Rose.Capabilities.Capability
   is (Set_Timer_Cap);

   --------------------------------
   -- Handle_Get_Timer_Interface --
   --------------------------------

   procedure Handle_Get_Timer_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Set_Timer_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Set_Timer_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Set_Timer_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Set_Timer_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Set_Timer_Endpoint, Identifier);
      else
         Cap := Set_Timer_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Timer_Interface;

   ----------------------
   -- Handle_Set_Timer --
   ----------------------

   procedure Handle_Set_Timer (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Milliseconds : constant Rose.Words.Word :=
         Rose.System_Calls.Get_Word_32 (Parameters, 0);
      Cap : constant Rose.Capabilities.Capability := Parameters.Caps (0);
      Result : constant Rose.Capabilities.Capability := Local_Set_Timer
         (Parameters.Identifier, Milliseconds, Cap);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Set_Timer;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Set_Timer      : in     Set_Timer_Handler)
   is
   begin
      Local_Set_Timer := Set_Timer;
      Set_Timer_Cap := Rose.Server.Create_Endpoint (Set_Timer_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Timer_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Timer_Interface,
         Handle_Get_Timer_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Set_Timer_Endpoint,
         Handle_Set_Timer'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Timer.Server;
