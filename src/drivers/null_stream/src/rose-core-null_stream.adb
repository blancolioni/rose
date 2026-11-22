with Null_Stream_Server;

procedure Rose.Core.Null_Stream
  (Served : out Rose.Capabilities.Capability)
is
begin
   Null_Stream_Server.Create_Server (Served);
end Rose.Core.Null_Stream;
