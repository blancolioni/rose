with Rights_Server;

procedure Rose.Core.Rights
  (Clock_Admin_Service :
     Rose.Interfaces.Clock_Admin.Client.Clock_Admin_Client;
   Rights              : out Rose.Capabilities.Capability;
   Admin               : out Rose.Capabilities.Capability)
is
begin
   Rights_Server.Create_Server (Clock_Admin_Service);
   Rights := Rights_Server.Rights_Cap;
   Admin := Rights_Server.Admin_Cap;
end Rose.Core.Rights;
