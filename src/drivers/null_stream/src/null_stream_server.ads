with Rose.Capabilities;

package Null_Stream_Server is

   --  The server of Rose.Core.Null_Stream (ADR 0035, section 4): one
   --  Stream, which reads as empty and discards what is written to it.

   procedure Create_Server
     (Served : out Rose.Capabilities.Capability);
   --  Attach the stream, and return its cap.

   procedure Start_Server;
   --  Serve it, for good.

end Null_Stream_Server;
