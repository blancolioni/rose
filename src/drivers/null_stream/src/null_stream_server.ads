with Rose.Capabilities;

package Null_Stream_Server is

   --  The server of Rose.Core.Null_Stream (ADR 0035, section 4): one
   --  Stream, which reads as empty and discards what is written to it,
   --  and one Writable_Directory, which has no entries and refuses every
   --  change (ADR 0040, section 5).

   procedure Create_Server
     (Served         : out Rose.Capabilities.Capability;
      Null_Directory : out Rose.Capabilities.Capability);
   --  Attach the stream and the directory, and return their caps.

   procedure Start_Server;
   --  Serve it, for good.

end Null_Stream_Server;
