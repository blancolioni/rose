with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Cap_Set.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Append : Append_Handler;
   Append_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Cap : Get_Cap_Handler;
   Get_Cap_Cap : Rose.Capabilities.Capability := 0;
   Local_Length : Length_Handler;
   Length_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Cap_Set_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Append (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Cap (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Length (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Append         : in     Append_Handler;
      Get_Cap        : in     Get_Cap_Handler;
      Length         : in     Length_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Append := Append;
      Local_Get_Cap := Get_Cap;
      Local_Length := Length;
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Set_Interface,
         Handle_Get_Cap_Set_Interface'Access);
      if not Instanced then
         Append_Cap := Rose.Server.Create_Endpoint (Append_Endpoint);
         Get_Cap_Cap := Rose.Server.Create_Endpoint (Get_Cap_Endpoint);
         Length_Cap := Rose.Server.Create_Endpoint (Length_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Cap_Set_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Append_Endpoint,
         Handle_Append'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Cap_Endpoint,
         Handle_Get_Cap'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Length_Endpoint,
         Handle_Length'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Append         : in     Append_Handler;
      Get_Cap        : in     Get_Cap_Handler;
      Length         : in     Length_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Append := Append;
      Local_Get_Cap := Get_Cap;
      Local_Length := Length;
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Set_Interface,
         Handle_Get_Cap_Set_Interface'Access);
      if not Instanced then
         Append_Cap := Rose.Server.Create_Endpoint (Append_Endpoint);
         Get_Cap_Cap := Rose.Server.Create_Endpoint (Get_Cap_Endpoint);
         Length_Cap := Rose.Server.Create_Endpoint (Length_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Cap_Set_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Append_Endpoint,
         Handle_Append'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Cap_Endpoint,
         Handle_Get_Cap'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Length_Endpoint,
         Handle_Length'Access);
   end Create_Server;

   --------------------
   -- Get_Append_Cap --
   --------------------

   function Get_Append_Cap return Rose.Capabilities.Capability
   is (Append_Cap);

   ---------------------
   -- Get_Get_Cap_Cap --
   ---------------------

   function Get_Get_Cap_Cap return Rose.Capabilities.Capability
   is (Get_Cap_Cap);

   --------------------
   -- Get_Length_Cap --
   --------------------

   function Get_Length_Cap return Rose.Capabilities.Capability
   is (Length_Cap);

   -------------------
   -- Handle_Append --
   -------------------

   procedure Handle_Append (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Caps : Rose.Capabilities.Capability_Array (1 .. 16);
      Cap_Count : constant Natural := Rose.System_Calls.Copy_Received_Caps
         (Parameters, 0, Caps);
   begin
      Local_Append (Parameters.Identifier, Caps (1 .. Cap_Count));
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Append;

   --------------------
   -- Handle_Get_Cap --
   --------------------

   procedure Handle_Get_Cap (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Index : constant Positive := Positive (Rose.System_Calls.Get_Word_32
         (Parameters, 0));
      Result : constant Rose.Capabilities.Capability := Local_Get_Cap
         (Parameters.Identifier, Index);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Get_Cap;

   ----------------------------------
   -- Handle_Get_Cap_Set_Interface --
   ----------------------------------

   procedure Handle_Get_Cap_Set_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Append_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Append_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Append_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Append_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Append_Endpoint, Identifier);
      else
         Cap := Append_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Get_Cap_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Get_Cap_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Get_Cap_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Get_Cap_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Get_Cap_Endpoint, Identifier);
      else
         Cap := Get_Cap_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Length_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Length_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Length_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Length_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Length_Endpoint, Identifier);
      else
         Cap := Length_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Cap_Set_Interface;

   -------------------
   -- Handle_Length --
   -------------------

   procedure Handle_Length (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Natural := Local_Length (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters, Result);
   end Handle_Length;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Append         : in     Append_Handler;
      Get_Cap        : in     Get_Cap_Handler;
      Length         : in     Length_Handler)
   is
   begin
      Local_Append := Append;
      Local_Get_Cap := Get_Cap;
      Local_Length := Length;
      Append_Cap := Rose.Server.Create_Endpoint (Append_Endpoint);
      Get_Cap_Cap := Rose.Server.Create_Endpoint (Get_Cap_Endpoint);
      Length_Cap := Rose.Server.Create_Endpoint (Length_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Cap_Set_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Cap_Set_Interface,
         Handle_Get_Cap_Set_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Append_Endpoint,
         Handle_Append'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Cap_Endpoint,
         Handle_Get_Cap'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Length_Endpoint,
         Handle_Length'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Cap_Set.Server;
