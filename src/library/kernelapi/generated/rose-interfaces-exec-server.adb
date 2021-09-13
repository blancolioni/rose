with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Exec.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Launch : Launch_Handler;
   Launch_Cap : Rose.Capabilities.Capability := 0;
   Local_Install : Install_Handler;
   Install_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Launch_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Exec_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Launch (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Install (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler;
      Install        : in     Install_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Launch := Launch;
      Local_Install := Install;
      Rose.Server.Register_Handler
        (Server_Context,
         Launch_Interface,
         Handle_Get_Launch_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Exec_Interface,
         Handle_Get_Exec_Interface'Access);
      if not Instanced then
         Launch_Cap := Rose.Server.Create_Endpoint (Launch_Endpoint);
         Install_Cap := Rose.Server.Create_Endpoint (Install_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Launch_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Exec_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Launch_Endpoint,
         Handle_Launch'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Install_Endpoint,
         Handle_Install'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler;
      Install        : in     Install_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Launch := Launch;
      Local_Install := Install;
      Rose.Server.Register_Handler
        (Server_Context,
         Launch_Interface,
         Handle_Get_Launch_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Exec_Interface,
         Handle_Get_Exec_Interface'Access);
      if not Instanced then
         Launch_Cap := Rose.Server.Create_Endpoint (Launch_Endpoint);
         Install_Cap := Rose.Server.Create_Endpoint (Install_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Launch_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Exec_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Launch_Endpoint,
         Handle_Launch'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Install_Endpoint,
         Handle_Install'Access);
   end Create_Server;

   ---------------------
   -- Get_Install_Cap --
   ---------------------

   function Get_Install_Cap return Rose.Capabilities.Capability
   is (Install_Cap);

   --------------------
   -- Get_Launch_Cap --
   --------------------

   function Get_Launch_Cap return Rose.Capabilities.Capability
   is (Launch_Cap);

   -------------------------------
   -- Handle_Get_Exec_Interface --
   -------------------------------

   procedure Handle_Get_Exec_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Launch_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Launch_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Launch_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Launch_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Launch_Endpoint, Identifier);
      else
         Cap := Launch_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Install_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Install_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Install_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Install_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Install_Endpoint, Identifier);
      else
         Cap := Install_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Exec_Interface;

   ---------------------------------
   -- Handle_Get_Launch_Interface --
   ---------------------------------

   procedure Handle_Get_Launch_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Launch_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Launch_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Launch_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Launch_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Launch_Endpoint, Identifier);
      else
         Cap := Launch_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Launch_Interface;

   --------------------
   -- Handle_Install --
   --------------------

   procedure Handle_Install (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Executable : constant Rose.Capabilities.Capability := Parameters.Caps
         (0);
      Caps : Rose.Capabilities.Capability_Array (1 .. 16);
      Cap_Count : constant Natural := Rose.System_Calls.Copy_Received_Caps
         (Parameters, 1, Caps);
      Result : constant Rose.Capabilities.Capability := Local_Install
         (Parameters.Identifier, Executable, Caps (1 .. Cap_Count));
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Install;

   -------------------
   -- Handle_Launch --
   -------------------

   procedure Handle_Launch (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Caps : Rose.Capabilities.Capability_Array (1 .. 16);
      Cap_Count : constant Natural := Rose.System_Calls.Copy_Received_Caps
         (Parameters, 0, Caps);
      Environment : System.Storage_Elements.Storage_Array
         (1 .. Parameters.Buffer_Length);
      pragma Import (Ada, Environment);
      for Environment'Address use Parameters.Buffer_Address;
      Result : constant Rose.Capabilities.Capability := Local_Launch
         (Parameters.Identifier, Caps (1 .. Cap_Count), Environment);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Launch;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Launch         : in     Launch_Handler;
      Install        : in     Install_Handler)
   is
   begin
      Local_Launch := Launch;
      Local_Install := Install;
      Launch_Cap := Rose.Server.Create_Endpoint (Launch_Endpoint);
      Install_Cap := Rose.Server.Create_Endpoint (Install_Endpoint);
      Rose.Server.Create_Anonymous_Endpoint (Launch_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Launch_Interface,
         Handle_Get_Launch_Interface'Access);
      Published_Cap := Rose.Server.Create_Endpoint (Exec_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Exec_Interface,
         Handle_Get_Exec_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Launch_Endpoint,
         Handle_Launch'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Install_Endpoint,
         Handle_Install'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Exec.Server;
