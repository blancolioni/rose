with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Stream_Writer.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Write : Write_Handler;
   Write_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Stream_Writer_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Write (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Write          : in     Write_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Write := Write;
      Rose.Server.Register_Handler
        (Server_Context,
         Stream_Writer_Interface,
         Handle_Get_Stream_Writer_Interface'Access);
      if not Instanced then
         Write_Cap := Rose.Server.Create_Endpoint (Write_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Stream_Writer_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Write_Endpoint,
         Handle_Write'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Write          : in     Write_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Write := Write;
      Rose.Server.Register_Handler
        (Server_Context,
         Stream_Writer_Interface,
         Handle_Get_Stream_Writer_Interface'Access);
      if not Instanced then
         Write_Cap := Rose.Server.Create_Endpoint (Write_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Stream_Writer_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Write_Endpoint,
         Handle_Write'Access);
   end Create_Server;

   -------------------
   -- Get_Write_Cap --
   -------------------

   function Get_Write_Cap return Rose.Capabilities.Capability
   is (Write_Cap);

   ----------------------------------------
   -- Handle_Get_Stream_Writer_Interface --
   ----------------------------------------

   procedure Handle_Get_Stream_Writer_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Write_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Write_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Write_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Write_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Write_Endpoint, Identifier);
      else
         Cap := Write_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Stream_Writer_Interface;

   ------------------
   -- Handle_Write --
   ------------------

   procedure Handle_Write (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Buffer : System.Storage_Elements.Storage_Array
         (1 .. Parameters.Buffer_Length);
      pragma Import (Ada, Buffer);
      for Buffer'Address use Parameters.Buffer_Address;
   begin
      Local_Write (Parameters.Identifier, Buffer);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Write;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Write          : in     Write_Handler)
   is
   begin
      Local_Write := Write;
      Write_Cap := Rose.Server.Create_Endpoint (Write_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Stream_Writer_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Stream_Writer_Interface,
         Handle_Get_Stream_Writer_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Write_Endpoint,
         Handle_Write'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Stream_Writer.Server;
