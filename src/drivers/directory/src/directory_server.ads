with Ada.Streams;

with Rose.Capabilities;
with Rose.Replacement;

package Directory_Server is

   --  The server of Rose.Core.Directory: one directory, served through
   --  Writable_Directory (ADR 0003). Each entry is a file or directory
   --  command, launched when the entry is created, in a bank of the
   --  entry's own; the directory holds its writable interface, and hands
   --  out read-only ones. A new version of the program can take over from
   --  a running one (ADR 0032, #831), and a saved state of a directory
   --  without entries can be restored (section 10).

   procedure Write_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps);
   --  The directory's state, for a new version or a save.

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps);
   --  The state of the version that this one takes over from, or of a
   --  saved one, which the main reads before it calls the command. It
   --  raises Bad_State if an entry's caps are missing, as they are from a
   --  saved state.

   procedure Create_Server
     (File_Launch      : Rose.Capabilities.Capability;
      Directory_Launch : Rose.Capabilities.Capability;
      Cap_Set_Service  : Rose.Capabilities.Capability;
      Served           : out Rose.Capabilities.Capability;
      Control          : out Rose.Capabilities.Capability);
   --  Attach the directory, with the identifier that the state had, and
   --  the Replaceable interface, and return their caps. The entries that
   --  the state had are the directory's. File_Launch and Directory_Launch
   --  launch new entries, and Cap_Set_Service is given to each.

   procedure Start_Server;
   --  Serve, and never return.

end Directory_Server;
