with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Server.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Published_Interface : Published_Interface_Handler;
   Published_Interface_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Server_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Published_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context      : in out Rose.Server.Server_Context;
      Published_Interface : in     Published_Interface_Handler;
      Instanced           : in     Boolean := False)
   is
   begin
      Local_Published_Interface := Published_Interface;
      Rose.Server.Register_Handler
        (Server_Context,
         Server_Interface,
         Handle_Get_Server_Interface'Access);
      if not Instanced then
         Published_Interface_Cap := Rose.Server.Create_Endpoint
            (Published_Interface_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Server_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Published_Interface_Endpoint,
         Handle_Published_Interface'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context      : in out Rose.Server.Server_Context;
      Published_Interface : in     Published_Interface_Handler;
      Instanced           : in     Boolean := False)
   is
   begin
      Local_Published_Interface := Published_Interface;
      Rose.Server.Register_Handler
        (Server_Context,
         Server_Interface,
         Handle_Get_Server_Interface'Access);
      if not Instanced then
         Published_Interface_Cap := Rose.Server.Create_Endpoint
            (Published_Interface_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Server_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Published_Interface_Endpoint,
         Handle_Published_Interface'Access);
   end Create_Server;

   ---------------------------------
   -- Get_Published_Interface_Cap --
   ---------------------------------

   function Get_Published_Interface_Cap return Rose.Capabilities.Capability
   is (Published_Interface_Cap);

   ---------------------------------
   -- Handle_Get_Server_Interface --
   ---------------------------------

   procedure Handle_Get_Server_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Published_Interface_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Published_Interface_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Published_Interface_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Published_Interface_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Published_Interface_Endpoint, Identifier);
      else
         Cap := Published_Interface_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Server_Interface;

   --------------------------------
   -- Handle_Published_Interface --
   --------------------------------

   procedure Handle_Published_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Capabilities.Capability :=
         Local_Published_Interface (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Published_Interface;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context      : in out Rose.Server.Server_Context;
      Published_Interface : in     Published_Interface_Handler)
   is
   begin
      Local_Published_Interface := Published_Interface;
      Published_Interface_Cap := Rose.Server.Create_Endpoint
         (Published_Interface_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Server_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Server_Interface,
         Handle_Get_Server_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Published_Interface_Endpoint,
         Handle_Published_Interface'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Server.Server;
