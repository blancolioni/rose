with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Map.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Add : Add_Handler;
   Add_Cap : Rose.Capabilities.Capability := 0;
   Local_Remove : Remove_Handler;
   Remove_Cap : Rose.Capabilities.Capability := 0;
   Local_Find : Find_Handler;
   Find_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Map_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Add (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Remove (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Find (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Add            : in     Add_Handler;
      Remove         : in     Remove_Handler;
      Find           : in     Find_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Add := Add;
      Local_Remove := Remove;
      Local_Find := Find;
      Rose.Server.Register_Handler
        (Server_Context,
         Map_Interface,
         Handle_Get_Map_Interface'Access);
      if not Instanced then
         Add_Cap := Rose.Server.Create_Endpoint (Add_Endpoint);
         Remove_Cap := Rose.Server.Create_Endpoint (Remove_Endpoint);
         Find_Cap := Rose.Server.Create_Endpoint (Find_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Map_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Endpoint,
         Handle_Add'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Remove_Endpoint,
         Handle_Remove'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Find_Endpoint,
         Handle_Find'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Add            : in     Add_Handler;
      Remove         : in     Remove_Handler;
      Find           : in     Find_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Add := Add;
      Local_Remove := Remove;
      Local_Find := Find;
      Rose.Server.Register_Handler
        (Server_Context,
         Map_Interface,
         Handle_Get_Map_Interface'Access);
      if not Instanced then
         Add_Cap := Rose.Server.Create_Endpoint (Add_Endpoint);
         Remove_Cap := Rose.Server.Create_Endpoint (Remove_Endpoint);
         Find_Cap := Rose.Server.Create_Endpoint (Find_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Map_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Endpoint,
         Handle_Add'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Remove_Endpoint,
         Handle_Remove'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Find_Endpoint,
         Handle_Find'Access);
   end Create_Server;

   ----------------
   -- Handle_Add --
   ----------------

   procedure Handle_Add (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Name : String (1 .. Natural (Parameters.Buffer_Length));
      pragma Import (Ada, Name);
      for Name'Address use Parameters.Buffer_Address;
      Cap : constant Rose.Capabilities.Capability := Parameters.Caps (0);
   begin
      Local_Add
        (Parameters.Identifier,
         Name,
         Cap);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Add;

   -----------------
   -- Handle_Find --
   -----------------

   procedure Handle_Find (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Name : String (1 .. Natural (Parameters.Buffer_Length));
      pragma Import (Ada, Name);
      for Name'Address use Parameters.Buffer_Address;
      Result : constant Rose.Capabilities.Capability := Local_Find
         (Parameters.Identifier, Name);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Find;

   ------------------------------
   -- Handle_Get_Map_Interface --
   ------------------------------

   procedure Handle_Get_Map_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Add_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Add_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Add_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Add_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance, Add_Endpoint,
            Identifier);
      else
         Cap := Add_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Remove_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Remove_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Remove_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Remove_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Remove_Endpoint, Identifier);
      else
         Cap := Remove_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Find_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Find_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Find_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Find_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance, Find_Endpoint,
            Identifier);
      else
         Cap := Find_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Map_Interface;

   -------------------
   -- Handle_Remove --
   -------------------

   procedure Handle_Remove (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Name : String (1 .. Natural (Parameters.Buffer_Length));
      pragma Import (Ada, Name);
      for Name'Address use Parameters.Buffer_Address;
   begin
      Local_Remove (Parameters.Identifier, Name);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Remove;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Add            : in     Add_Handler;
      Remove         : in     Remove_Handler;
      Find           : in     Find_Handler)
   is
   begin
      Local_Add := Add;
      Local_Remove := Remove;
      Local_Find := Find;
      Add_Cap := Rose.Server.Create_Endpoint (Add_Endpoint);
      Remove_Cap := Rose.Server.Create_Endpoint (Remove_Endpoint);
      Find_Cap := Rose.Server.Create_Endpoint (Find_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Map_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Map_Interface,
         Handle_Get_Map_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Add_Endpoint,
         Handle_Add'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Remove_Endpoint,
         Handle_Remove'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Find_Endpoint,
         Handle_Find'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Map.Server;
