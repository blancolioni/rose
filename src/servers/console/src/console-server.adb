with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Objects;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Output_Redirect;
with Rose.Interfaces.Stream_Writer.Client;
with Rose.Interfaces.Stream_Writer.Server;

with Console.IO;

package body Console.Server is

   Server_Context : Rose.Server.Server_Context;

   procedure Handle_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array);

   procedure Handle_Redirect
     (Parameters : in out Rose.Invocation.Invocation_Record);
   --  Output_Redirect.Redirect.  The boot console registers this endpoint
   --  itself rather than through the generated server package, whose
   --  instance table would take pages of the kernel heap before mem runs.

   Console_Buffer_Size : constant := 4096;

   Console_Buffer      : System.Storage_Elements.Storage_Array
     (1 .. Console_Buffer_Size)
     with Alignment => 4096;

   --  Once a persistent console takes over the screen, writes go to it
   --  instead, or nowhere if there is no target.

   Redirected     : Boolean := False;
   Has_Target     : Boolean := False;
   Target_Console : Rose.Interfaces.Stream_Writer.Client.Stream_Writer_Client;

   ---------------------------
   -- Create_Console_Server --
   ---------------------------

   procedure Create_Console_Server is
   begin
      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => 1);
      Rose.System_Calls.Use_Buffer
        (Console_Buffer'Address, Console_Buffer_Size);

      Rose.Interfaces.Stream_Writer.Server.Create_Server
        (Server_Context => Server_Context,
         Write          => Handle_Write'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Rose.Interfaces.Output_Redirect.Redirect_Endpoint,
         Handle_Redirect'Access);
   end Create_Console_Server;

   ---------------------
   -- Handle_Redirect --
   ---------------------

   procedure Handle_Redirect
     (Parameters : in out Rose.Invocation.Invocation_Record)
   is
      use type Rose.Capabilities.Capability;
      Target : constant Rose.Capabilities.Capability :=
                 (if Parameters.Control.Flags (Rose.Invocation.Send_Caps)
                  then Parameters.Caps (0)
                  else Rose.Capabilities.Null_Capability);
   begin
      Redirected := True;
      Has_Target := Target /= Rose.Capabilities.Null_Capability;
      if Has_Target then
         Rose.Interfaces.Stream_Writer.Client.Open (Target_Console, Target);
      end if;
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
   end Handle_Redirect;

   ------------------
   -- Handle_Write --
   ------------------

   procedure Handle_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      S : String (1 .. Natural (Buffer'Length));
      for S'Address use Buffer'Address;
      pragma Import (Ada, S);
   begin
      if not Redirected then
         Console.IO.Put (S);
      elsif Has_Target then
         Rose.Interfaces.Stream_Writer.Client.Write (Target_Console, Buffer);
      end if;
   end Handle_Write;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end Console.Server;
