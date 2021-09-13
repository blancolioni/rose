with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Process.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Add_Segment : Add_Segment_Handler;
   Add_Segment_Cap : Rose.Capabilities.Capability := 0;
   Local_Add_Nonpersistent_Segment : Add_Nonpersistent_Segment_Handler;
   Add_Nonpersistent_Segment_Cap : Rose.Capabilities.Capability := 0;
   Local_Published_Interface : Published_Interface_Handler;
   Published_Interface_Cap : Rose.Capabilities.Capability := 0;
   Local_Destroy : Destroy_Handler;
   Destroy_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Object_Id : Get_Object_Id_Handler;
   Get_Object_Id_Cap : Rose.Capabilities.Capability := 0;
   Local_Heap_Interface : Heap_Interface_Handler;
   Heap_Interface_Cap : Rose.Capabilities.Capability := 0;
   Local_Exit_Process : Exit_Process_Handler;
   Exit_Process_Cap : Rose.Capabilities.Capability := 0;
   Local_Publish_Interface : Publish_Interface_Handler;
   Publish_Interface_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Segment_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Server_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Cap_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Process_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Add_Segment (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Add_Nonpersistent_Segment (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Published_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Destroy (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Object_Id (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Heap_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Exit_Process (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Publish_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Published_Interface       : in     Published_Interface_Handler;
      Destroy                   : in     Destroy_Handler;
      Get_Object_Id             : in     Get_Object_Id_Handler;
      Heap_Interface            : in     Heap_Interface_Handler;
      Exit_Process              : in     Exit_Process_Handler;
      Publish_Interface         : in     Publish_Interface_Handler;
      Instanced                 : in     Boolean := False)
   is
   begin
      Local_Add_Segment := Add_Segment;
      Local_Add_Nonpersistent_Segment := Add_Nonpersistent_Segment;
      Local_Published_Interface := Published_Interface;
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Local_Heap_Interface := Heap_Interface;
      Local_Exit_Process := Exit_Process;
      Local_Publish_Interface := Publish_Interface;
      Rose.Server.Register_Handler
        (Server_Context,
         Segment_Interface,
         Handle_Get_Segment_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Server_Interface,
         Handle_Get_Server_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Process_Interface,
         Handle_Get_Process_Interface'Access);
      if not Instanced then
         Add_Segment_Cap := Rose.Server.Create_Endpoint
            (Add_Segment_Endpoint);
         Add_Nonpersistent_Segment_Cap := Rose.Server.Create_Endpoint
            (Add_Nonpersistent_Segment_Endpoint);
         Published_Interface_Cap := Rose.Server.Create_Endpoint
            (Published_Interface_Endpoint);
         Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
         Get_Object_Id_Cap := Rose.Server.Create_Endpoint
            (Get_Object_Id_Endpoint);
         Heap_Interface_Cap := Rose.Server.Create_Endpoint
            (Heap_Interface_Endpoint);
         Exit_Process_Cap := Rose.Server.Create_Endpoint
            (Exit_Process_Endpoint);
         Publish_Interface_Cap := Rose.Server.Create_Endpoint
            (Publish_Interface_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Segment_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Server_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Cap_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Process_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Segment_Endpoint,
         Handle_Add_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Nonpersistent_Segment_Endpoint,
         Handle_Add_Nonpersistent_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Published_Interface_Endpoint,
         Handle_Published_Interface'Access);
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
         Heap_Interface_Endpoint,
         Handle_Heap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Exit_Process_Endpoint,
         Handle_Exit_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Publish_Interface_Endpoint,
         Handle_Publish_Interface'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Published_Interface       : in     Published_Interface_Handler;
      Destroy                   : in     Destroy_Handler;
      Get_Object_Id             : in     Get_Object_Id_Handler;
      Heap_Interface            : in     Heap_Interface_Handler;
      Exit_Process              : in     Exit_Process_Handler;
      Publish_Interface         : in     Publish_Interface_Handler;
      Instanced                 : in     Boolean := False)
   is
   begin
      Local_Add_Segment := Add_Segment;
      Local_Add_Nonpersistent_Segment := Add_Nonpersistent_Segment;
      Local_Published_Interface := Published_Interface;
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Local_Heap_Interface := Heap_Interface;
      Local_Exit_Process := Exit_Process;
      Local_Publish_Interface := Publish_Interface;
      Rose.Server.Register_Handler
        (Server_Context,
         Segment_Interface,
         Handle_Get_Segment_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Server_Interface,
         Handle_Get_Server_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Process_Interface,
         Handle_Get_Process_Interface'Access);
      if not Instanced then
         Add_Segment_Cap := Rose.Server.Create_Endpoint
            (Add_Segment_Endpoint);
         Add_Nonpersistent_Segment_Cap := Rose.Server.Create_Endpoint
            (Add_Nonpersistent_Segment_Endpoint);
         Published_Interface_Cap := Rose.Server.Create_Endpoint
            (Published_Interface_Endpoint);
         Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
         Get_Object_Id_Cap := Rose.Server.Create_Endpoint
            (Get_Object_Id_Endpoint);
         Heap_Interface_Cap := Rose.Server.Create_Endpoint
            (Heap_Interface_Endpoint);
         Exit_Process_Cap := Rose.Server.Create_Endpoint
            (Exit_Process_Endpoint);
         Publish_Interface_Cap := Rose.Server.Create_Endpoint
            (Publish_Interface_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Segment_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Server_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Cap_Interface);
         Rose.Server.Create_Anonymous_Endpoint (Process_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Segment_Endpoint,
         Handle_Add_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Nonpersistent_Segment_Endpoint,
         Handle_Add_Nonpersistent_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Published_Interface_Endpoint,
         Handle_Published_Interface'Access);
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
         Heap_Interface_Endpoint,
         Handle_Heap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Exit_Process_Endpoint,
         Handle_Exit_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Publish_Interface_Endpoint,
         Handle_Publish_Interface'Access);
   end Create_Server;

   ---------------------------------------
   -- Get_Add_Nonpersistent_Segment_Cap --
   ---------------------------------------

   function Get_Add_Nonpersistent_Segment_Cap
      return Rose.Capabilities.Capability
   is (Add_Nonpersistent_Segment_Cap);

   -------------------------
   -- Get_Add_Segment_Cap --
   -------------------------

   function Get_Add_Segment_Cap return Rose.Capabilities.Capability
   is (Add_Segment_Cap);

   ---------------------
   -- Get_Destroy_Cap --
   ---------------------

   function Get_Destroy_Cap return Rose.Capabilities.Capability
   is (Destroy_Cap);

   --------------------------
   -- Get_Exit_Process_Cap --
   --------------------------

   function Get_Exit_Process_Cap return Rose.Capabilities.Capability
   is (Exit_Process_Cap);

   ---------------------------
   -- Get_Get_Object_Id_Cap --
   ---------------------------

   function Get_Get_Object_Id_Cap return Rose.Capabilities.Capability
   is (Get_Object_Id_Cap);

   ----------------------------
   -- Get_Heap_Interface_Cap --
   ----------------------------

   function Get_Heap_Interface_Cap return Rose.Capabilities.Capability
   is (Heap_Interface_Cap);

   -------------------------------
   -- Get_Publish_Interface_Cap --
   -------------------------------

   function Get_Publish_Interface_Cap return Rose.Capabilities.Capability
   is (Publish_Interface_Cap);

   ---------------------------------
   -- Get_Published_Interface_Cap --
   ---------------------------------

   function Get_Published_Interface_Cap return Rose.Capabilities.Capability
   is (Published_Interface_Cap);

   --------------------------------------
   -- Handle_Add_Nonpersistent_Segment --
   --------------------------------------

   procedure Handle_Add_Nonpersistent_Segment (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Virtual_Base : constant Rose.Words.Word :=
         Rose.System_Calls.Get_Word_32 (Parameters, 0);
      Virtual_Bound : constant Rose.Words.Word :=
         Rose.System_Calls.Get_Word_32 (Parameters, 1);
      Flags : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 2);
   begin
      Local_Add_Nonpersistent_Segment
        (Parameters.Identifier,
         Virtual_Base,
         Virtual_Bound,
         Flags);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Add_Nonpersistent_Segment;

   ------------------------
   -- Handle_Add_Segment --
   ------------------------

   procedure Handle_Add_Segment (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Virtual_Base : constant Rose.Words.Word :=
         Rose.System_Calls.Get_Word_32 (Parameters, 0);
      Virtual_Bound : constant Rose.Words.Word :=
         Rose.System_Calls.Get_Word_32 (Parameters, 1);
      Region : constant Rose.Capabilities.Capability := Parameters.Caps (0);
      Region_Offset : constant Rose.Words.Word :=
         Rose.System_Calls.Get_Word_32 (Parameters, 2);
      Flags : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 3);
   begin
      Local_Add_Segment
        (Parameters.Identifier,
         Virtual_Base,
         Virtual_Bound,
         Region,
         Region_Offset,
         Flags);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Add_Segment;

   --------------------
   -- Handle_Destroy --
   --------------------

   procedure Handle_Destroy (Parameters :
      in out Rose.Invocation.Invocation_Record) is
   begin
      Local_Destroy (Parameters.Identifier);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Destroy;

   -------------------------
   -- Handle_Exit_Process --
   -------------------------

   procedure Handle_Exit_Process (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Exit_Status : constant Natural := Natural
         (Rose.System_Calls.Get_Word_32 (Parameters, 0));
   begin
      Local_Exit_Process (Parameters.Identifier, Exit_Status);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Exit_Process;

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

   ----------------------------------
   -- Handle_Get_Process_Interface --
   ----------------------------------

   procedure Handle_Get_Process_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Add_Segment_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Add_Segment_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Add_Segment_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Add_Segment_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Add_Segment_Endpoint, Identifier);
      else
         Cap := Add_Segment_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Add_Nonpersistent_Segment_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Add_Nonpersistent_Segment_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Add_Nonpersistent_Segment_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint
                  (Add_Nonpersistent_Segment_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Add_Nonpersistent_Segment_Endpoint, Identifier);
      else
         Cap := Add_Nonpersistent_Segment_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Published_Interface_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Published_Interface_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Published_Interface_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Published_Interface_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Published_Interface_Endpoint, Identifier);
      else
         Cap := Published_Interface_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
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
      if Heap_Interface_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Heap_Interface_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Heap_Interface_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Heap_Interface_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Heap_Interface_Endpoint, Identifier);
      else
         Cap := Heap_Interface_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Exit_Process_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Exit_Process_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Exit_Process_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Exit_Process_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Exit_Process_Endpoint, Identifier);
      else
         Cap := Exit_Process_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Publish_Interface_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Publish_Interface_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Publish_Interface_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Publish_Interface_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Publish_Interface_Endpoint, Identifier);
      else
         Cap := Publish_Interface_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Process_Interface;

   ----------------------------------
   -- Handle_Get_Segment_Interface --
   ----------------------------------

   procedure Handle_Get_Segment_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Add_Segment_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Add_Segment_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Add_Segment_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Add_Segment_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Add_Segment_Endpoint, Identifier);
      else
         Cap := Add_Segment_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Add_Nonpersistent_Segment_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Add_Nonpersistent_Segment_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Add_Nonpersistent_Segment_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint
                  (Add_Nonpersistent_Segment_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Add_Nonpersistent_Segment_Endpoint, Identifier);
      else
         Cap := Add_Nonpersistent_Segment_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Segment_Interface;

   ---------------------------------
   -- Handle_Get_Server_Interface --
   ---------------------------------

   procedure Handle_Get_Server_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Published_Interface_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Published_Interface_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Published_Interface_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Published_Interface_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Published_Interface_Endpoint, Identifier);
      else
         Cap := Published_Interface_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Server_Interface;

   ---------------------------
   -- Handle_Heap_Interface --
   ---------------------------

   procedure Handle_Heap_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Capabilities.Capability := Local_Heap_Interface
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Heap_Interface;

   ------------------------------
   -- Handle_Publish_Interface --
   ------------------------------

   procedure Handle_Publish_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Interface_Cap : constant Rose.Capabilities.Capability :=
         Parameters.Caps (0);
   begin
      Local_Publish_Interface (Parameters.Identifier, Interface_Cap);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Publish_Interface;

   --------------------------------
   -- Handle_Published_Interface --
   --------------------------------

   procedure Handle_Published_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Capabilities.Capability :=
         Local_Published_Interface (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Published_Interface;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Published_Interface       : in     Published_Interface_Handler;
      Destroy                   : in     Destroy_Handler;
      Get_Object_Id             : in     Get_Object_Id_Handler;
      Heap_Interface            : in     Heap_Interface_Handler;
      Exit_Process              : in     Exit_Process_Handler;
      Publish_Interface         : in     Publish_Interface_Handler)
   is
   begin
      Local_Add_Segment := Add_Segment;
      Local_Add_Nonpersistent_Segment := Add_Nonpersistent_Segment;
      Local_Published_Interface := Published_Interface;
      Local_Destroy := Destroy;
      Local_Get_Object_Id := Get_Object_Id;
      Local_Heap_Interface := Heap_Interface;
      Local_Exit_Process := Exit_Process;
      Local_Publish_Interface := Publish_Interface;
      Add_Segment_Cap := Rose.Server.Create_Endpoint (Add_Segment_Endpoint);
      Add_Nonpersistent_Segment_Cap := Rose.Server.Create_Endpoint
         (Add_Nonpersistent_Segment_Endpoint);
      Published_Interface_Cap := Rose.Server.Create_Endpoint
         (Published_Interface_Endpoint);
      Destroy_Cap := Rose.Server.Create_Endpoint (Destroy_Endpoint);
      Get_Object_Id_Cap := Rose.Server.Create_Endpoint
         (Get_Object_Id_Endpoint);
      Heap_Interface_Cap := Rose.Server.Create_Endpoint
         (Heap_Interface_Endpoint);
      Exit_Process_Cap := Rose.Server.Create_Endpoint
         (Exit_Process_Endpoint);
      Publish_Interface_Cap := Rose.Server.Create_Endpoint
         (Publish_Interface_Endpoint);
      Rose.Server.Create_Anonymous_Endpoint (Segment_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Segment_Interface,
         Handle_Get_Segment_Interface'Access);
      Rose.Server.Create_Anonymous_Endpoint (Server_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Server_Interface,
         Handle_Get_Server_Interface'Access);
      Rose.Server.Create_Anonymous_Endpoint (Cap_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Interface,
         Handle_Get_Cap_Interface'Access);
      Published_Cap := Rose.Server.Create_Endpoint (Process_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Process_Interface,
         Handle_Get_Process_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Segment_Endpoint,
         Handle_Add_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Nonpersistent_Segment_Endpoint,
         Handle_Add_Nonpersistent_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Published_Interface_Endpoint,
         Handle_Published_Interface'Access);
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
         Heap_Interface_Endpoint,
         Handle_Heap_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Exit_Process_Endpoint,
         Handle_Exit_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Publish_Interface_Endpoint,
         Handle_Publish_Interface'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Process.Server;
