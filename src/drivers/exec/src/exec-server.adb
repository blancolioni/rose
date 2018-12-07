with Rose.Invocation;
with Rose.Objects;

with Rose.Console_IO;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Exec.Server;

package body Exec.Server is

   function On_Install
     (Id        : Rose.Objects.Capability_Identifier;
      ELF_Image : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   procedure On_Launch
     (Id   : Rose.Objects.Capability_Identifier;
      Caps : Rose.Capabilities.Capability_Array);

   Context : Rose.Server.Server_Context;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Console_IO.Put_Line ("exec: creating server");
      Rose.Interfaces.Exec.Server.Create_Server
        (Server_Context => Context,
         Install        => On_Install'Access,
         Launch         => On_Launch'Access);
   end Create_Server;

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
