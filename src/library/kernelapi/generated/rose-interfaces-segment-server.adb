with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Segment.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Add_Segment : Add_Segment_Handler;
   Add_Segment_Cap : Rose.Capabilities.Capability := 0;
   Local_Add_Nonpersistent_Segment : Add_Nonpersistent_Segment_Handler;
   Add_Nonpersistent_Segment_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Segment_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Add_Segment (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Add_Nonpersistent_Segment (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Instanced                 : in     Boolean := False)
   is
   begin
      Local_Add_Segment := Add_Segment;
      Local_Add_Nonpersistent_Segment := Add_Nonpersistent_Segment;
      Rose.Server.Register_Handler
        (Server_Context,
         Segment_Interface,
         Handle_Get_Segment_Interface'Access);
      if not Instanced then
         Add_Segment_Cap := Rose.Server.Create_Endpoint
            (Add_Segment_Endpoint);
         Add_Nonpersistent_Segment_Cap := Rose.Server.Create_Endpoint
            (Add_Nonpersistent_Segment_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Segment_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Segment_Endpoint,
         Handle_Add_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Nonpersistent_Segment_Endpoint,
         Handle_Add_Nonpersistent_Segment'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Instanced                 : in     Boolean := False)
   is
   begin
      Local_Add_Segment := Add_Segment;
      Local_Add_Nonpersistent_Segment := Add_Nonpersistent_Segment;
      Rose.Server.Register_Handler
        (Server_Context,
         Segment_Interface,
         Handle_Get_Segment_Interface'Access);
      if not Instanced then
         Add_Segment_Cap := Rose.Server.Create_Endpoint
            (Add_Segment_Endpoint);
         Add_Nonpersistent_Segment_Cap := Rose.Server.Create_Endpoint
            (Add_Nonpersistent_Segment_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Segment_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Segment_Endpoint,
         Handle_Add_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Nonpersistent_Segment_Endpoint,
         Handle_Add_Nonpersistent_Segment'Access);
   end Create_Server;

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

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler)
   is
   begin
      Local_Add_Segment := Add_Segment;
      Local_Add_Nonpersistent_Segment := Add_Nonpersistent_Segment;
      Add_Segment_Cap := Rose.Server.Create_Endpoint (Add_Segment_Endpoint);
      Add_Nonpersistent_Segment_Cap := Rose.Server.Create_Endpoint
         (Add_Nonpersistent_Segment_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Segment_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Segment_Interface,
         Handle_Get_Segment_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Segment_Endpoint,
         Handle_Add_Segment'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Nonpersistent_Segment_Endpoint,
         Handle_Add_Nonpersistent_Segment'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Segment.Server;
