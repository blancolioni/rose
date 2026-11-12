with Replaceable_Test_Server;

procedure Rose.Core.Replaceable_Test
  (Cap_Set_Service : in out
     Rose.Interfaces.Constructor.Constructor_Object'Class;
   Name            : String;
   Served          : out Rose.Capabilities.Capability;
   Control         : out Rose.Capabilities.Capability;
   Saver           : out Rose.Capabilities.Capability)
is
begin
   Replaceable_Test_Server.Create_Server
     (Name            => Name,
      Cap_Set_Service => Cap_Set_Service.Constructor_Cap,
      Served          => Served,
      Control         => Control,
      Saver           => Saver);
end Rose.Core.Replaceable_Test;
