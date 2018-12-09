with Rose.Objects;
with Rose.Server;

with Rose.Console_IO;

with Rose.Interfaces.Executable.Server;

package body Elf.Server is

   Server_Context : Rose.Server.Server_Context;

   procedure Launch
     (Id    : Rose.Objects.Capability_Identifier;
      Image : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Executable.Server.Create_Server
        (Server_Context => Server_Context,
         Launch         => Launch'Access);
   end Create_Server;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Id    : Rose.Objects.Capability_Identifier;
      Image : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array)
   is
      pragma Unreferenced (Id, Image, Caps);
   begin
      Rose.Console_IO.Put_Line ("elf: launching image");
   end Launch;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Console_IO.Put_Line ("elf: starting server");
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end Elf.Server;
