with Page_Probe_Server;

procedure Rose.Core.Page_Probe
  (Pages      : Natural;
   Checkpoint : in out Rose.Interfaces.Caps.Cap.Cap_Object'Class;
   Target     : in out Rose.Interfaces.Streams.Writer.Writer_Object'Class;
   Writer     : out Rose.Capabilities.Capability;
   Probe      : out Rose.Capabilities.Capability)
is
begin
   Page_Probe_Server.Create_Server
     (Pages      => Pages,
      Checkpoint => Checkpoint.Cap_Cap,
      Target     => Target.Writer_Cap);
   Writer := Page_Probe_Server.Writer_Cap;
   Probe := Page_Probe_Server.Named_Cap;
end Rose.Core.Page_Probe;
