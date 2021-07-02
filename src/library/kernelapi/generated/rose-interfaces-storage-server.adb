with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Storage.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Reserve_Storage : Reserve_Storage_Handler;
   Reserve_Storage_Cap : Rose.Capabilities.Capability := 0;
   Local_Add_Backing_Store : Add_Backing_Store_Handler;
   Add_Backing_Store_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Storage_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Reserve_Storage (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Add_Backing_Store (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context    : in out Rose.Server.Server_Context;
      Reserve_Storage   : in     Reserve_Storage_Handler;
      Add_Backing_Store : in     Add_Backing_Store_Handler;
      Instanced         : in     Boolean := False)
   is
   begin
      Local_Reserve_Storage := Reserve_Storage;
      Local_Add_Backing_Store := Add_Backing_Store;
      Rose.Server.Register_Handler
        (Server_Context,
         Storage_Interface,
         Handle_Get_Storage_Interface'Access);
      if not Instanced then
         Reserve_Storage_Cap := Rose.Server.Create_Endpoint
            (Reserve_Storage_Endpoint);
         Add_Backing_Store_Cap := Rose.Server.Create_Endpoint
            (Add_Backing_Store_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Storage_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Reserve_Storage_Endpoint,
         Handle_Reserve_Storage'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Backing_Store_Endpoint,
         Handle_Add_Backing_Store'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context    : in out Rose.Server.Server_Context;
      Reserve_Storage   : in     Reserve_Storage_Handler;
      Add_Backing_Store : in     Add_Backing_Store_Handler;
      Instanced         : in     Boolean := False)
   is
   begin
      Local_Reserve_Storage := Reserve_Storage;
      Local_Add_Backing_Store := Add_Backing_Store;
      Rose.Server.Register_Handler
        (Server_Context,
         Storage_Interface,
         Handle_Get_Storage_Interface'Access);
      if not Instanced then
         Reserve_Storage_Cap := Rose.Server.Create_Endpoint
            (Reserve_Storage_Endpoint);
         Add_Backing_Store_Cap := Rose.Server.Create_Endpoint
            (Add_Backing_Store_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Storage_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Reserve_Storage_Endpoint,
         Handle_Reserve_Storage'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Backing_Store_Endpoint,
         Handle_Add_Backing_Store'Access);
   end Create_Server;

   ------------------------------
   -- Handle_Add_Backing_Store --
   ------------------------------

   procedure Handle_Add_Backing_Store (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Store : constant Rose.Capabilities.Capability := Parameters.Caps (0);
   begin
      Local_Add_Backing_Store (Parameters.Identifier, Store);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Add_Backing_Store;

   ----------------------------------
   -- Handle_Get_Storage_Interface --
   ----------------------------------

   procedure Handle_Get_Storage_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Reserve_Storage_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Reserve_Storage_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Reserve_Storage_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Reserve_Storage_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Reserve_Storage_Endpoint, Identifier);
      else
         Cap := Reserve_Storage_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Add_Backing_Store_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Add_Backing_Store_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Add_Backing_Store_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Add_Backing_Store_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Add_Backing_Store_Endpoint, Identifier);
      else
         Cap := Add_Backing_Store_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Storage_Interface;

   ----------------------------
   -- Handle_Reserve_Storage --
   ----------------------------

   procedure Handle_Reserve_Storage (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Size : constant Rose.Words.Word_64 := Rose.System_Calls.Get_Word_64
         (Parameters, 0);
      Result : constant Rose.Capabilities.Capability := Local_Reserve_Storage
         (Parameters.Identifier, Size);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Reserve_Storage;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context    : in out Rose.Server.Server_Context;
      Reserve_Storage   : in     Reserve_Storage_Handler;
      Add_Backing_Store : in     Add_Backing_Store_Handler)
   is
   begin
      Local_Reserve_Storage := Reserve_Storage;
      Local_Add_Backing_Store := Add_Backing_Store;
      Reserve_Storage_Cap := Rose.Server.Create_Endpoint
         (Reserve_Storage_Endpoint);
      Add_Backing_Store_Cap := Rose.Server.Create_Endpoint
         (Add_Backing_Store_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Storage_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Storage_Interface,
         Handle_Get_Storage_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Reserve_Storage_Endpoint,
         Handle_Reserve_Storage'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Backing_Store_Endpoint,
         Handle_Add_Backing_Store'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Storage.Server;
