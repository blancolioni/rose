with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;
with Rose.Capabilities;

package body Rose.Interfaces.Keyboard_Handler.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Handle_Key : Handle_Key_Handler;
   Handle_Key_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Keyboard_Handler_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Handle_Key (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Handle_Key     : in     Handle_Key_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Handle_Key := Handle_Key;
      Rose.Server.Register_Handler
        (Server_Context,
         Keyboard_Handler_Interface,
         Handle_Get_Keyboard_Handler_Interface'Access);
      if not Instanced then
         Handle_Key_Cap := Rose.Server.Create_Endpoint (Handle_Key_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Keyboard_Handler_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Handle_Key_Endpoint,
         Handle_Handle_Key'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Handle_Key     : in     Handle_Key_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Handle_Key := Handle_Key;
      Rose.Server.Register_Handler
        (Server_Context,
         Keyboard_Handler_Interface,
         Handle_Get_Keyboard_Handler_Interface'Access);
      if not Instanced then
         Handle_Key_Cap := Rose.Server.Create_Endpoint (Handle_Key_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Keyboard_Handler_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Handle_Key_Endpoint,
         Handle_Handle_Key'Access);
   end Create_Server;

   -------------------------------------------
   -- Handle_Get_Keyboard_Handler_Interface --
   -------------------------------------------

   procedure Handle_Get_Keyboard_Handler_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Handle_Key_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Handle_Key_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Handle_Key_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Handle_Key_Endpoint,
               Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Handle_Key_Endpoint, Identifier);
      else
         Cap := Handle_Key_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Keyboard_Handler_Interface;

   -----------------------
   -- Handle_Handle_Key --
   -----------------------

   procedure Handle_Handle_Key (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Code : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 0);
      State : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 1);
   begin
      Local_Handle_Key
        (Parameters.Identifier,
         Code,
         State);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Handle_Key;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Handle_Key     : in     Handle_Key_Handler)
   is
   begin
      Local_Handle_Key := Handle_Key;
      Handle_Key_Cap := Rose.Server.Create_Endpoint (Handle_Key_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint
         (Keyboard_Handler_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Keyboard_Handler_Interface,
         Handle_Get_Keyboard_Handler_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Handle_Key_Endpoint,
         Handle_Handle_Key'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Keyboard_Handler.Server;
