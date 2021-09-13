with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Ata.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Device : Get_Device_Handler;
   Get_Device_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Ata_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Device (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Device     : in     Get_Device_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Get_Device := Get_Device;
      Rose.Server.Register_Handler
        (Server_Context,
         Ata_Interface,
         Handle_Get_Ata_Interface'Access);
      if not Instanced then
         Get_Device_Cap := Rose.Server.Create_Endpoint (Get_Device_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Ata_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Device_Endpoint,
         Handle_Get_Device'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Device     : in     Get_Device_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Get_Device := Get_Device;
      Rose.Server.Register_Handler
        (Server_Context,
         Ata_Interface,
         Handle_Get_Ata_Interface'Access);
      if not Instanced then
         Get_Device_Cap := Rose.Server.Create_Endpoint (Get_Device_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Ata_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Device_Endpoint,
         Handle_Get_Device'Access);
   end Create_Server;

   ------------------------
   -- Get_Get_Device_Cap --
   ------------------------

   function Get_Get_Device_Cap return Rose.Capabilities.Capability
   is (Get_Device_Cap);

   ------------------------------
   -- Handle_Get_Ata_Interface --
   ------------------------------

   procedure Handle_Get_Ata_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Get_Device_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Get_Device_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Get_Device_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Get_Device_Endpoint,
               Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Get_Device_Endpoint, Identifier);
      else
         Cap := Get_Device_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Ata_Interface;

   -----------------------
   -- Handle_Get_Device --
   -----------------------

   procedure Handle_Get_Device (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Capabilities.Capability := Local_Get_Device
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Get_Device;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Get_Device     : in     Get_Device_Handler)
   is
   begin
      Local_Get_Device := Get_Device;
      Get_Device_Cap := Rose.Server.Create_Endpoint (Get_Device_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Ata_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Ata_Interface,
         Handle_Get_Ata_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Device_Endpoint,
         Handle_Get_Device'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Ata.Server;
