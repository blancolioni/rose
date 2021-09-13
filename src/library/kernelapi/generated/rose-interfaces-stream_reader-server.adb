with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Stream_Reader.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Read : Read_Handler;
   Read_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Stream_Reader_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Read (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Read := Read;
      Rose.Server.Register_Handler
        (Server_Context,
         Stream_Reader_Interface,
         Handle_Get_Stream_Reader_Interface'Access);
      if not Instanced then
         Read_Cap := Rose.Server.Create_Endpoint (Read_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Stream_Reader_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Read_Endpoint,
         Handle_Read'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Read := Read;
      Rose.Server.Register_Handler
        (Server_Context,
         Stream_Reader_Interface,
         Handle_Get_Stream_Reader_Interface'Access);
      if not Instanced then
         Read_Cap := Rose.Server.Create_Endpoint (Read_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Stream_Reader_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Read_Endpoint,
         Handle_Read'Access);
   end Create_Server;

   ------------------
   -- Get_Read_Cap --
   ------------------

   function Get_Read_Cap return Rose.Capabilities.Capability
   is (Read_Cap);

   ----------------------------------------
   -- Handle_Get_Stream_Reader_Interface --
   ----------------------------------------

   procedure Handle_Get_Stream_Reader_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
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
   end Handle_Get_Stream_Reader_Interface;

   -----------------
   -- Handle_Read --
   -----------------

   procedure Handle_Read (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Buffer : System.Storage_Elements.Storage_Array
         (1 .. Parameters.Buffer_Length);
      pragma Import (Ada, Buffer);
      for Buffer'Address use Parameters.Buffer_Address;
      Last : System.Storage_Elements.Storage_Count;
   begin
      Local_Read
        (Parameters.Identifier,
         Buffer,
         Last);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Storage_Offset (Parameters, Last);
   end Handle_Read;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Read           : in     Read_Handler)
   is
   begin
      Local_Read := Read;
      Read_Cap := Rose.Server.Create_Endpoint (Read_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Stream_Reader_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Stream_Reader_Interface,
         Handle_Get_Stream_Reader_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Read_Endpoint,
         Handle_Read'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Stream_Reader.Server;
