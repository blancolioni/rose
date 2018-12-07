with Rose.Invocation;
with Rose.Objects;

with Rose.Console_IO;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Exec.Server;
with Rose.Interfaces.Storage.Client;
with Rose.Interfaces.Stream_Reader.Client;

with Exec.Library;

package body Exec.Server is

   function On_Install
     (Id        : Rose.Objects.Capability_Identifier;
      ELF_Image : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   procedure On_Launch
     (Id   : Rose.Objects.Capability_Identifier;
      Caps : Rose.Capabilities.Capability_Array);

   Context : Rose.Server.Server_Context;
   Storage : Rose.Interfaces.Storage.Client.Storage_Client;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Console_IO.Put_Line ("exec: creating server");
      Rose.Interfaces.Storage.Client.Open (Storage, Storage_Cap);

      Rose.Interfaces.Exec.Server.Create_Server
        (Server_Context => Context,
         Install        => On_Install'Access,
         Launch         => On_Launch'Access);
   end Create_Server;

   ----------------
   -- On_Install --
   ----------------

   function On_Install
     (Id        : Rose.Objects.Capability_Identifier;
      ELF_Image : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      use Rose.Interfaces.Stream_Reader.Client;
      Reader : Stream_Reader_Client;
   begin
      Open (Reader, ELF_Image);
      return Exec.Library.Install (Storage, Reader);
   end On_Install;

   ---------------
   -- On_Launch --
   ---------------

   procedure On_Launch
     (Id   : in     Rose.Objects.Capability_Identifier;
      Caps : Rose.Capabilities.Capability_Array)
   is
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Create_Process_Cap);
      for Cap of Caps loop
         Send_Cap (Params, Cap);
      end loop;
      Invoke_Capability (Params);
   end On_Launch;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Exec.Server;
