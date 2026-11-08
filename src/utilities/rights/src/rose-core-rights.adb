with Rights_Server;

procedure Rose.Core.Rights
  (Clock_Admin_Service : in out
     Rose.Interfaces.Clock_Admin.Clock_Admin_Object'Class;
   Rights              : out Rose.Capabilities.Capability;
   Admin               : out Rose.Capabilities.Capability)
is
begin
   Rights_Server.Create_Server (Clock_Admin_Service.Clock_Admin_Cap);
   Rights := Rights_Server.Rights_Cap;
   Admin := Rights_Server.Admin_Cap;
end Rose.Core.Rights;
