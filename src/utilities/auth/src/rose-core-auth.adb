with Auth_Server;

procedure Rose.Core.Auth
  (Find_Command : in out Rose.Interfaces.Commands.Commands_Object'Class;
   Auth         : out Rose.Capabilities.Capability;
   Admin        : out Rose.Capabilities.Capability)
is
begin
   Auth_Server.Create_Server (Find_Command.Commands_Cap);
   Auth := Auth_Server.Auth_Cap;
   Admin := Auth_Server.Admin_Cap;
end Rose.Core.Auth;
