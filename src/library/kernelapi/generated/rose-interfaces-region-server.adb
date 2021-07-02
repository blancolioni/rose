with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Region.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Range : Get_Range_Handler;
   Get_Range_Cap : Rose.Capabilities.Capability := 0;
   Local_Get : Get_Handler;
   Get_Cap : Rose.Capabilities.Capability := 0;
   Local_Put : Put_Handler;
   Put_Cap : Rose.Capabilities.Capability := 0;
   Local_Read : Read_Handler;
   Read_Cap : Rose.Capabilities.Capability := 0;
   Local_Create_Subregion : Create_Subregion_Handler;
   Create_Subregion_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Region_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Range (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Put (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Read (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Create_Subregion (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context   : in out Rose.Server.Server_Context;
      Get_Range        : in     Get_Range_Handler;
      Get              : in     Get_Handler;
      Put              : in     Put_Handler;
      Read             : in     Read_Handler;
      Create_Subregion : in     Create_Subregion_Handler;
      Instanced        : in     Boolean := False)
   is
   begin
      Local_Get_Range := Get_Range;
      Local_Get := Get;
      Local_Put := Put;
      Local_Read := Read;
      Local_Create_Subregion := Create_Subregion;
      Rose.Server.Register_Handler
        (Server_Context,
         Region_Interface,
         Handle_Get_Region_Interface'Access);
      if not Instanced then
         Get_Range_Cap := Rose.Server.Create_Endpoint (Get_Range_Endpoint);
         Get_Cap := Rose.Server.Create_Endpoint (Get_Endpoint);
         Put_Cap := Rose.Server.Create_Endpoint (Put_Endpoint);
         Read_Cap := Rose.Server.Create_Endpoint (Read_Endpoint);
         Create_Subregion_Cap := Rose.Server.Create_Endpoint
            (Create_Subregion_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Region_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Range_Endpoint,
         Handle_Get_Range'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Endpoint,
         Handle_Get'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Put_Endpoint,
         Handle_Put'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Read_Endpoint,
         Handle_Read'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Subregion_Endpoint,
         Handle_Create_Subregion'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context   : in out Rose.Server.Server_Context;
      Get_Range        : in     Get_Range_Handler;
      Get              : in     Get_Handler;
      Put              : in     Put_Handler;
      Read             : in     Read_Handler;
      Create_Subregion : in     Create_Subregion_Handler;
      Instanced        : in     Boolean := False)
   is
   begin
      Local_Get_Range := Get_Range;
      Local_Get := Get;
      Local_Put := Put;
      Local_Read := Read;
      Local_Create_Subregion := Create_Subregion;
      Rose.Server.Register_Handler
        (Server_Context,
         Region_Interface,
         Handle_Get_Region_Interface'Access);
      if not Instanced then
         Get_Range_Cap := Rose.Server.Create_Endpoint (Get_Range_Endpoint);
         Get_Cap := Rose.Server.Create_Endpoint (Get_Endpoint);
         Put_Cap := Rose.Server.Create_Endpoint (Put_Endpoint);
         Read_Cap := Rose.Server.Create_Endpoint (Read_Endpoint);
         Create_Subregion_Cap := Rose.Server.Create_Endpoint
            (Create_Subregion_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Region_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Range_Endpoint,
         Handle_Get_Range'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Endpoint,
         Handle_Get'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Put_Endpoint,
         Handle_Put'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Read_Endpoint,
         Handle_Read'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Subregion_Endpoint,
         Handle_Create_Subregion'Access);
   end Create_Server;

   -----------------------------
   -- Handle_Create_Subregion --
   -----------------------------

   procedure Handle_Create_Subregion (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Subregion_Base : constant Rose.Objects.Object_Id :=
         Rose.Objects.Object_Id (Rose.System_Calls.Get_Word_64 (Parameters,
         0));
      Subregion_Bound : constant Rose.Objects.Object_Id :=
         Rose.Objects.Object_Id (Rose.System_Calls.Get_Word_64 (Parameters,
         2));
      Flags : constant Rose.Words.Word := Rose.System_Calls.Get_Word_32
         (Parameters, 4);
      Result : constant Rose.Capabilities.Capability :=
         Local_Create_Subregion (Parameters.Identifier, Subregion_Base,
         Subregion_Bound, Flags);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Create_Subregion;

   ----------------
   -- Handle_Get --
   ----------------

   procedure Handle_Get (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Page : constant Rose.Objects.Object_Id := Rose.Objects.Object_Id
         (Rose.System_Calls.Get_Word_64 (Parameters, 0));
      Data : System.Storage_Elements.Storage_Array
         (1 .. Parameters.Buffer_Length);
      pragma Import (Ada, Data);
      for Data'Address use Parameters.Buffer_Address;
   begin
      Local_Get
        (Parameters.Identifier,
         Page,
         Data);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Get;

   ----------------------
   -- Handle_Get_Range --
   ----------------------

   procedure Handle_Get_Range (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Base_Page : Rose.Objects.Object_Id;
      Bound_Page : Rose.Objects.Object_Id;
   begin
      Local_Get_Range
        (Parameters.Identifier,
         Base_Page,
         Bound_Page);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Object_Id (Parameters, Base_Page);
      Rose.System_Calls.Send_Object_Id (Parameters, Bound_Page);
   end Handle_Get_Range;

   ---------------------------------
   -- Handle_Get_Region_Interface --
   ---------------------------------

   procedure Handle_Get_Region_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Get_Range_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Get_Range_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Get_Range_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Get_Range_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Get_Range_Endpoint, Identifier);
      else
         Cap := Get_Range_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Get_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Get_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Get_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Get_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance, Get_Endpoint,
            Identifier);
      else
         Cap := Get_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Put_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Put_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Put_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Put_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance, Put_Endpoint,
            Identifier);
      else
         Cap := Put_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Read_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Read_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Read_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Read_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance, Read_Endpoint,
            Identifier);
      else
         Cap := Read_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Create_Subregion_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Create_Subregion_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Create_Subregion_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Create_Subregion_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Create_Subregion_Endpoint, Identifier);
      else
         Cap := Create_Subregion_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Region_Interface;

   ----------------
   -- Handle_Put --
   ----------------

   procedure Handle_Put (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Page : constant Rose.Objects.Object_Id := Rose.Objects.Object_Id
         (Rose.System_Calls.Get_Word_64 (Parameters, 0));
      Data : System.Storage_Elements.Storage_Array
         (1 .. Parameters.Buffer_Length);
      pragma Import (Ada, Data);
      for Data'Address use Parameters.Buffer_Address;
   begin
      Local_Put
        (Parameters.Identifier,
         Page,
         Data);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Put;

   -----------------
   -- Handle_Read --
   -----------------

   procedure Handle_Read (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Capabilities.Capability := Local_Read
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Read;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context   : in out Rose.Server.Server_Context;
      Get_Range        : in     Get_Range_Handler;
      Get              : in     Get_Handler;
      Put              : in     Put_Handler;
      Read             : in     Read_Handler;
      Create_Subregion : in     Create_Subregion_Handler)
   is
   begin
      Local_Get_Range := Get_Range;
      Local_Get := Get;
      Local_Put := Put;
      Local_Read := Read;
      Local_Create_Subregion := Create_Subregion;
      Get_Range_Cap := Rose.Server.Create_Endpoint (Get_Range_Endpoint);
      Get_Cap := Rose.Server.Create_Endpoint (Get_Endpoint);
      Put_Cap := Rose.Server.Create_Endpoint (Put_Endpoint);
      Read_Cap := Rose.Server.Create_Endpoint (Read_Endpoint);
      Create_Subregion_Cap := Rose.Server.Create_Endpoint
         (Create_Subregion_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Region_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Region_Interface,
         Handle_Get_Region_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Range_Endpoint,
         Handle_Get_Range'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Endpoint,
         Handle_Get'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Put_Endpoint,
         Handle_Put'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Read_Endpoint,
         Handle_Read'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Subregion_Endpoint,
         Handle_Create_Subregion'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Region.Server;
