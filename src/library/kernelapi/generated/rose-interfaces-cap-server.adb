with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Cap.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Destroy : Destroy_Handler;
   Destroy_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Object_Id : Get_Object_Id_Handler;
   Get_Object_Id_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Cap_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Destroy (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Object_Id (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      if not Instanced then
         Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
         Get_Object_Id_Cap := Rose.Server.Create_Endpoint
            (Get_Object_Id_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Cap_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Destroy_Endpoint,
         Handle_Destroy'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Object_Id_Endpoint,
         Handle_Get_Object_Id'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      if not Instanced then
         Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
         Get_Object_Id_Cap := Rose.Server.Create_Endpoint
            (Get_Object_Id_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Cap_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Destroy_Endpoint,
         Handle_Destroy'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Object_Id_Endpoint,
         Handle_Get_Object_Id'Access);
   end Create_Server;

   ---------------------
   -- Get_Destroy_Cap --
   ---------------------

   function Get_Destroy_Cap return Rose.Capabilities.Capability
   is (Destroy_Cap);

   ---------------------------
   -- Get_Get_Object_Id_Cap --
   ---------------------------

   function Get_Get_Object_Id_Cap return Rose.Capabilities.Capability
   is (Get_Object_Id_Cap);

   --------------------
   -- Handle_Destroy --
   --------------------

   procedure Handle_Destroy (Parameters :
      in out Rose.Invocation.Invocation_Record) is
   begin
      Local_Destroy (Parameters.Identifier);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Destroy;

   ------------------------------
   -- Handle_Get_Cap_Interface --
   ------------------------------

   procedure Handle_Get_Cap_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Destroy_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Destroy_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Destroy_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Destroy_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Destroy_Endpoint, Identifier);
      else
         Cap := Destroy_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Get_Object_Id_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Get_Object_Id_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Get_Object_Id_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Get_Object_Id_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Get_Object_Id_Endpoint, Identifier);
      else
         Cap := Get_Object_Id_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Cap_Interface;

   --------------------------
   -- Handle_Get_Object_Id --
   --------------------------

   procedure Handle_Get_Object_Id (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Objects.Object_Id := Local_Get_Object_Id
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Object_Id (Parameters, Result);
   end Handle_Get_Object_Id;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler)
   is
   begin
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
      Get_Object_Id_Cap := Rose.Server.Create_Endpoint
         (Get_Object_Id_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Cap_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Destroy_Endpoint,
         Handle_Destroy'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Object_Id_Endpoint,
         Handle_Get_Object_Id'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Cap.Server;
