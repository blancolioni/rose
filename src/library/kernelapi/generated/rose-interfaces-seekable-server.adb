with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;
with Rose.Capabilities;

package body Rose.Interfaces.Seekable.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Seek : Seek_Handler;
   Seek_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Seekable_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Seek (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Seek           : in     Seek_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Seek := Seek;
      Rose.Server.Register_Handler
        (Server_Context,
         Seekable_Interface,
         Handle_Get_Seekable_Interface'Access);
      if not Instanced then
         Seek_Cap := Rose.Server.Create_Endpoint (Seek_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Seekable_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Seek_Endpoint,
         Handle_Seek'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Seek           : in     Seek_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Seek := Seek;
      Rose.Server.Register_Handler
        (Server_Context,
         Seekable_Interface,
         Handle_Get_Seekable_Interface'Access);
      if not Instanced then
         Seek_Cap := Rose.Server.Create_Endpoint (Seek_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Seekable_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Seek_Endpoint,
         Handle_Seek'Access);
   end Create_Server;

   -----------------------------------
   -- Handle_Get_Seekable_Interface --
   -----------------------------------

   procedure Handle_Get_Seekable_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Seek_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance, Seek_Endpoint,
            Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Seek_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Seek_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance, Seek_Endpoint,
            Identifier);
      else
         Cap := Seek_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Seekable_Interface;

   -----------------
   -- Handle_Seek --
   -----------------

   procedure Handle_Seek (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Offset_From_Start : constant Rose.Words.Word_64 :=
         Rose.System_Calls.Get_Word_64 (Parameters, 0);
   begin
      Local_Seek (Parameters.Identifier, Offset_From_Start);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Seek;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Seek           : in     Seek_Handler)
   is
   begin
      Local_Seek := Seek;
      Seek_Cap := Rose.Server.Create_Endpoint (Seek_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Seekable_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Seekable_Interface,
         Handle_Get_Seekable_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Seek_Endpoint,
         Handle_Seek'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Seekable.Server;
