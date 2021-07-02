with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Constructor.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Create : Create_Handler;
   Create_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Constructor_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Create (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Create         : in     Create_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Create := Create;
      Rose.Server.Register_Handler
        (Server_Context,
         Constructor_Interface,
         Handle_Get_Constructor_Interface'Access);
      if not Instanced then
         Create_Cap := Rose.Server.Create_Endpoint (Create_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Constructor_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Endpoint,
         Handle_Create'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Create         : in     Create_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Create := Create;
      Rose.Server.Register_Handler
        (Server_Context,
         Constructor_Interface,
         Handle_Get_Constructor_Interface'Access);
      if not Instanced then
         Create_Cap := Rose.Server.Create_Endpoint (Create_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Constructor_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Endpoint,
         Handle_Create'Access);
   end Create_Server;

   -------------------
   -- Handle_Create --
   -------------------

   procedure Handle_Create (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Capabilities.Capability := Local_Create
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Create;

   --------------------------------------
   -- Handle_Get_Constructor_Interface --
   --------------------------------------

   procedure Handle_Get_Constructor_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Create_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Create_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Create_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Create_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Create_Endpoint, Identifier);
      else
         Cap := Create_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Constructor_Interface;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Create         : in     Create_Handler)
   is
   begin
      Local_Create := Create;
      Create_Cap := Rose.Server.Create_Endpoint (Create_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Constructor_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Constructor_Interface,
         Handle_Get_Constructor_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Endpoint,
         Handle_Create'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Constructor.Server;
