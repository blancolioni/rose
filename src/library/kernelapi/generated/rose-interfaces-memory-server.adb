with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Memory.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_New_Process : New_Process_Handler;
   New_Process_Cap : Rose.Capabilities.Capability := 0;
   Local_Register_Process : Register_Process_Handler;
   Register_Process_Cap : Rose.Capabilities.Capability := 0;
   Local_Page_Fault : Page_Fault_Handler;
   Page_Fault_Cap : Rose.Capabilities.Capability := 0;
   Local_Take_Physical_Memory : Take_Physical_Memory_Handler;
   Take_Physical_Memory_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Memory_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_New_Process (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Register_Process (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Page_Fault (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Take_Physical_Memory (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context       : in out Rose.Server.Server_Context;
      New_Process          : in     New_Process_Handler;
      Register_Process     : in     Register_Process_Handler;
      Page_Fault           : in     Page_Fault_Handler;
      Take_Physical_Memory : in     Take_Physical_Memory_Handler;
      Instanced            : in     Boolean := False)
   is
   begin
      Local_New_Process := New_Process;
      Local_Register_Process := Register_Process;
      Local_Page_Fault := Page_Fault;
      Local_Take_Physical_Memory := Take_Physical_Memory;
      Rose.Server.Register_Handler
        (Server_Context,
         Memory_Interface,
         Handle_Get_Memory_Interface'Access);
      if not Instanced then
         New_Process_Cap := Rose.Server.Create_Endpoint
            (New_Process_Endpoint);
         Register_Process_Cap := Rose.Server.Create_Endpoint
            (Register_Process_Endpoint);
         Page_Fault_Cap := Rose.Server.Create_Endpoint (Page_Fault_Endpoint);
         Take_Physical_Memory_Cap := Rose.Server.Create_Endpoint
            (Take_Physical_Memory_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Memory_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         New_Process_Endpoint,
         Handle_New_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Register_Process_Endpoint,
         Handle_Register_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Page_Fault_Endpoint,
         Handle_Page_Fault'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Take_Physical_Memory_Endpoint,
         Handle_Take_Physical_Memory'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context       : in out Rose.Server.Server_Context;
      New_Process          : in     New_Process_Handler;
      Register_Process     : in     Register_Process_Handler;
      Page_Fault           : in     Page_Fault_Handler;
      Take_Physical_Memory : in     Take_Physical_Memory_Handler;
      Instanced            : in     Boolean := False)
   is
   begin
      Local_New_Process := New_Process;
      Local_Register_Process := Register_Process;
      Local_Page_Fault := Page_Fault;
      Local_Take_Physical_Memory := Take_Physical_Memory;
      Rose.Server.Register_Handler
        (Server_Context,
         Memory_Interface,
         Handle_Get_Memory_Interface'Access);
      if not Instanced then
         New_Process_Cap := Rose.Server.Create_Endpoint
            (New_Process_Endpoint);
         Register_Process_Cap := Rose.Server.Create_Endpoint
            (Register_Process_Endpoint);
         Page_Fault_Cap := Rose.Server.Create_Endpoint (Page_Fault_Endpoint);
         Take_Physical_Memory_Cap := Rose.Server.Create_Endpoint
            (Take_Physical_Memory_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Memory_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         New_Process_Endpoint,
         Handle_New_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Register_Process_Endpoint,
         Handle_Register_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Page_Fault_Endpoint,
         Handle_Page_Fault'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Take_Physical_Memory_Endpoint,
         Handle_Take_Physical_Memory'Access);
   end Create_Server;

   ---------------------------------
   -- Handle_Get_Memory_Interface --
   ---------------------------------

   procedure Handle_Get_Memory_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if New_Process_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            New_Process_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               New_Process_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (New_Process_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            New_Process_Endpoint, Identifier);
      else
         Cap := New_Process_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Register_Process_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Register_Process_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Register_Process_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Register_Process_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Register_Process_Endpoint, Identifier);
      else
         Cap := Register_Process_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Page_Fault_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Page_Fault_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Page_Fault_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Page_Fault_Endpoint,
               Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Page_Fault_Endpoint, Identifier);
      else
         Cap := Page_Fault_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Take_Physical_Memory_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Take_Physical_Memory_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Take_Physical_Memory_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Take_Physical_Memory_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Take_Physical_Memory_Endpoint, Identifier);
      else
         Cap := Take_Physical_Memory_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Memory_Interface;

   ------------------------
   -- Handle_New_Process --
   ------------------------

   procedure Handle_New_Process (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Process : constant Rose.Capabilities.Capability := Parameters.Caps (0);
      Result : constant Rose.Capabilities.Capability := Local_New_Process
         (Parameters.Identifier, Process);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_New_Process;

   -----------------------
   -- Handle_Page_Fault --
   -----------------------

   procedure Handle_Page_Fault (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Faulting_Object : constant Rose.Objects.Object_Id :=
         Rose.Objects.Object_Id (Rose.System_Calls.Get_Word_64 (Parameters,
         0));
      Virtual_Page : constant Rose.Words.Word :=
         Rose.System_Calls.Get_Word_32 (Parameters, 2);
      Physical_Page : constant Rose.Words.Word :=
         Rose.System_Calls.Get_Word_32 (Parameters, 3);
      Action : constant Rose.Interfaces.Memory.Page_Access_Type :=
         Rose.Interfaces.Memory.Page_Access_Type'Val
         (Rose.System_Calls.Get_Word_32 (Parameters, 4));
   begin
      Local_Page_Fault
        (Parameters.Identifier,
         Faulting_Object,
         Virtual_Page,
         Physical_Page,
         Action);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Page_Fault;

   -----------------------------
   -- Handle_Register_Process --
   -----------------------------

   procedure Handle_Register_Process (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Process : constant Rose.Capabilities.Capability := Parameters.Caps (0);
      Exec_Base : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 0);
      Exec_Bound : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 1);
      Text_Base : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 2);
      Text_Bound : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 3);
      Data_Base : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 4);
      Data_Bound : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 5);
      Stack_Base : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 6);
      Stack_Bound : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 7);
      Environment : System.Storage_Elements.Storage_Array
         (1 .. Parameters.Buffer_Length);
      pragma Import (Ada, Environment);
      for Environment'Address use Parameters.Buffer_Address;
   begin
      Local_Register_Process
        (Parameters.Identifier,
         Process,
         Exec_Base,
         Exec_Bound,
         Text_Base,
         Text_Bound,
         Data_Base,
         Data_Bound,
         Stack_Base,
         Stack_Bound,
         Environment);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Register_Process;

   ---------------------------------
   -- Handle_Take_Physical_Memory --
   ---------------------------------

   procedure Handle_Take_Physical_Memory (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Size : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 0);
      Start : Rose.Words.Word;
      Amount : Rose.Words.Word;
   begin
      Local_Take_Physical_Memory
        (Parameters.Identifier,
         Size,
         Start,
         Amount);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters, Start);
      Rose.System_Calls.Send_Word (Parameters, Amount);
   end Handle_Take_Physical_Memory;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context       : in out Rose.Server.Server_Context;
      New_Process          : in     New_Process_Handler;
      Register_Process     : in     Register_Process_Handler;
      Page_Fault           : in     Page_Fault_Handler;
      Take_Physical_Memory : in     Take_Physical_Memory_Handler)
   is
   begin
      Local_New_Process := New_Process;
      Local_Register_Process := Register_Process;
      Local_Page_Fault := Page_Fault;
      Local_Take_Physical_Memory := Take_Physical_Memory;
      New_Process_Cap := Rose.Server.Create_Endpoint (New_Process_Endpoint);
      Register_Process_Cap := Rose.Server.Create_Endpoint
         (Register_Process_Endpoint);
      Page_Fault_Cap := Rose.Server.Create_Endpoint (Page_Fault_Endpoint);
      Take_Physical_Memory_Cap := Rose.Server.Create_Endpoint
         (Take_Physical_Memory_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Memory_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Memory_Interface,
         Handle_Get_Memory_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         New_Process_Endpoint,
         Handle_New_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Register_Process_Endpoint,
         Handle_Register_Process'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Page_Fault_Endpoint,
         Handle_Page_Fault'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Take_Physical_Memory_Endpoint,
         Handle_Take_Physical_Memory'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Memory.Server;
