with Null_Stream_Server;

procedure Rose.Core.Null_Stream
  (Served         : out Rose.Capabilities.Capability;
   Null_Directory : out Rose.Capabilities.Capability)
is
begin
   Null_Stream_Server.Create_Server (Served, Null_Directory);
end Rose.Core.Null_Stream;
