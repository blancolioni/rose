with Ada.Streams;

with Rose.Capabilities;
with Rose.Replacement;

package File_Server is

   --  The server of Rose.Core.File: one file, at most one version of it
   --  (ADR 0026), and the readers and writers opened on them (ADR 0009).
   --  A new version of the program can take over from a running one
   --  (ADR 0032, #867), and a saved state can be restored (section 10).

   procedure Write_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps);
   --  The file's state, for a new version or a save.

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps);
   --  The state of the version that this one takes over from, or of a
   --  saved one, which the main reads before it calls the command.

   procedure Create_Server
     (Cap_Set_Service : Rose.Capabilities.Capability;
      Served          : out Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability);
   --  Attach the file, and the version, readers and writers that the
   --  state had, each with its old identifier, and the Replaceable
   --  interface, and return the caps of the file's Writable_File and of
   --  Replaceable. Cap_Set_Service is used only by a server that neither
   --  took over nor restored.

   procedure Start_Server;
   --  Serve, and never return.

end File_Server;
