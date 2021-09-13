with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.File_System.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Root_Directory : Root_Directory_Handler;
   Root_Directory_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_File_System_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Root_Directory (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Root_Directory : in     Root_Directory_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Root_Directory := Root_Directory;
      Rose.Server.Register_Handler
        (Server_Context,
         File_System_Interface,
         Handle_Get_File_System_Interface'Access);
      if not Instanced then
         Root_Directory_Cap := Rose.Server.Create_Endpoint
            (Root_Directory_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (File_System_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Root_Directory_Endpoint,
         Handle_Root_Directory'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context : in out Rose.Server.Server_Context;
      Root_Directory : in     Root_Directory_Handler;
      Instanced      : in     Boolean := False)
   is
   begin
      Local_Root_Directory := Root_Directory;
      Rose.Server.Register_Handler
        (Server_Context,
         File_System_Interface,
         Handle_Get_File_System_Interface'Access);
      if not Instanced then
         Root_Directory_Cap := Rose.Server.Create_Endpoint
            (Root_Directory_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (File_System_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Root_Directory_Endpoint,
         Handle_Root_Directory'Access);
   end Create_Server;

   ----------------------------
   -- Get_Root_Directory_Cap --
   ----------------------------

   function Get_Root_Directory_Cap return Rose.Capabilities.Capability
   is (Root_Directory_Cap);

   --------------------------------------
   -- Handle_Get_File_System_Interface --
   --------------------------------------

   procedure Handle_Get_File_System_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Root_Directory_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Root_Directory_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Root_Directory_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Root_Directory_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Root_Directory_Endpoint, Identifier);
      else
         Cap := Root_Directory_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_File_System_Interface;

   ---------------------------
   -- Handle_Root_Directory --
   ---------------------------

   procedure Handle_Root_Directory (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Rose.Capabilities.Capability := Local_Root_Directory
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Root_Directory;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context : in out Rose.Server.Server_Context;
      Root_Directory : in     Root_Directory_Handler)
   is
   begin
      Local_Root_Directory := Root_Directory;
      Root_Directory_Cap := Rose.Server.Create_Endpoint
         (Root_Directory_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (File_System_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         File_System_Interface,
         Handle_Get_File_System_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Root_Directory_Endpoint,
         Handle_Root_Directory'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.File_System.Server;
