with Auth_Server;

procedure Rose.Core.Auth
  (Find_Command : Rose.Interfaces.Commands.Client.Commands_Client;
   Auth         : out Rose.Capabilities.Capability;
   Admin        : out Rose.Capabilities.Capability)
is
begin
   Auth_Server.Create_Server (Find_Command);
   Auth := Auth_Server.Auth_Cap;
   Admin := Auth_Server.Admin_Cap;
end Rose.Core.Auth;
