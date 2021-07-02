with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;
with Rose.Capabilities;

package body Rose.Interfaces.Kernel_Process.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Destroy : Destroy_Handler;
   Destroy_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Object_Id : Get_Object_Id_Handler;
   Get_Object_Id_Cap : Rose.Capabilities.Capability := 0;
   Local_Resume : Resume_Handler;
   Resume_Cap : Rose.Capabilities.Capability := 0;
   Local_Fault : Fault_Handler;
   Fault_Cap : Rose.Capabilities.Capability := 0;
   Local_Notify : Notify_Handler;
   Notify_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Cap_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Kernel_Process_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Destroy (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Object_Id (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Resume (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Fault (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Notify (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler;
      Resume         : in     Resume_Handler;
      Fault          : in     Fault_Handler;
      Notify         : in     Notify_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Local_Resume := Resume;
      Local_Fault := Fault;
      Local_Notify := Notify;
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Kernel_Process_Interface,
         Handle_Get_Kernel_Process_Interface'Access);
      if not Instanced then
         Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
         Get_Object_Id_Cap := Rose.Server.Create_Endpoint
            (Get_Object_Id_Endpoint);
         Resume_Cap := Rose.Server.Create_Endpoint (Resume_Endpoint);
         Fault_Cap := Rose.Server.Create_Endpoint (Fault_Endpoint);
         Notify_Cap := Rose.Server.Create_Endpoint (Notify_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Cap_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Kernel_Process_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Destroy_Endpoint,
         Handle_Destroy'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Object_Id_Endpoint,
         Handle_Get_Object_Id'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Resume_Endpoint,
         Handle_Resume'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Fault_Endpoint,
         Handle_Fault'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Notify_Endpoint,
         Handle_Notify'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler;
      Resume         : in     Resume_Handler;
      Fault          : in     Fault_Handler;
      Notify         : in     Notify_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Local_Resume := Resume;
      Local_Fault := Fault;
      Local_Notify := Notify;
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Kernel_Process_Interface,
         Handle_Get_Kernel_Process_Interface'Access);
      if not Instanced then
         Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
         Get_Object_Id_Cap := Rose.Server.Create_Endpoint
            (Get_Object_Id_Endpoint);
         Resume_Cap := Rose.Server.Create_Endpoint (Resume_Endpoint);
         Fault_Cap := Rose.Server.Create_Endpoint (Fault_Endpoint);
         Notify_Cap := Rose.Server.Create_Endpoint (Notify_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Cap_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Kernel_Process_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Destroy_Endpoint,
         Handle_Destroy'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Object_Id_Endpoint,
         Handle_Get_Object_Id'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Resume_Endpoint,
         Handle_Resume'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Fault_Endpoint,
         Handle_Fault'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Notify_Endpoint,
         Handle_Notify'Access);
   end Create_Server;

   --------------------
   -- Handle_Destroy --
   --------------------

   procedure Handle_Destroy (Parameters :
      in out Rose.Invocation.Invocation_Record) is
   begin
      Local_Destroy (Parameters.Identifier);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Destroy;

   ------------------
   -- Handle_Fault --
   ------------------

   procedure Handle_Fault (Parameters :
      in out Rose.Invocation.Invocation_Record) is
   begin
      Local_Fault (Parameters.Identifier);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Fault;

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

   -----------------------------------------
   -- Handle_Get_Kernel_Process_Interface --
   -----------------------------------------

   procedure Handle_Get_Kernel_Process_Interface (Parameters :
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
      if Resume_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Resume_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Resume_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Resume_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Resume_Endpoint, Identifier);
      else
         Cap := Resume_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Fault_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Fault_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Fault_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Fault_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Fault_Endpoint, Identifier);
      else
         Cap := Fault_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Notify_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Notify_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Notify_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Notify_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Notify_Endpoint, Identifier);
      else
         Cap := Notify_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Kernel_Process_Interface;

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

   -------------------
   -- Handle_Notify --
   -------------------

   procedure Handle_Notify (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Flags : constant Rose.Words.Word_32 := Rose.System_Calls.Get_Word_32
         (Parameters, 0);
   begin
      Local_Notify (Parameters.Identifier, Flags);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Notify;

   -------------------
   -- Handle_Resume --
   -------------------

   procedure Handle_Resume (Parameters :
      in out Rose.Invocation.Invocation_Record) is
   begin
      Local_Resume (Parameters.Identifier);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Resume;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Destroy        : in     Destroy_Handler;
      Get_Object_Id  : in     Get_Object_Id_Handler;
      Resume         : in     Resume_Handler;
      Fault          : in     Fault_Handler;
      Notify         : in     Notify_Handler)
   is
   begin
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Local_Resume := Resume;
      Local_Fault := Fault;
      Local_Notify := Notify;
      Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
      Get_Object_Id_Cap := Rose.Server.Create_Endpoint
         (Get_Object_Id_Endpoint);
      Resume_Cap := Rose.Server.Create_Endpoint (Resume_Endpoint);
      Fault_Cap := Rose.Server.Create_Endpoint (Fault_Endpoint);
      Notify_Cap := Rose.Server.Create_Endpoint (Notify_Endpoint);
      Rose.Server.Create_Anonymous_Endpoint (Cap_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      Published_Cap := Rose.Server.Create_Endpoint
         (Kernel_Process_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Kernel_Process_Interface,
         Handle_Get_Kernel_Process_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Destroy_Endpoint,
         Handle_Destroy'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Object_Id_Endpoint,
         Handle_Get_Object_Id'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Resume_Endpoint,
         Handle_Resume'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Fault_Endpoint,
         Handle_Fault'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Notify_Endpoint,
         Handle_Notify'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Kernel_Process.Server;
