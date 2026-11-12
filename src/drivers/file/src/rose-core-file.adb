with File_Server;

procedure Rose.Core.File
  (Cap_Set_Service : in out
     Rose.Interfaces.Constructor.Constructor_Object'Class;
   Served          : out Rose.Capabilities.Capability;
   Control         : out Rose.Capabilities.Capability)
is
begin
   File_Server.Create_Server
     (Cap_Set_Service => Cap_Set_Service.Constructor_Cap,
      Served          => Served,
      Control         => Control);
end Rose.Core.File;
