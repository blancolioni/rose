with Replaceable_Test_Server;

procedure Rose.Core.Replaceable_Test
  (Cap_Set_Service : in out
     Rose.Interfaces.Constructor.Constructor_Object'Class;
   Name            : String;
   Target          : in out Rose.Interfaces.Held_Call.Held_Call_Object'Class;
   Served          : out Rose.Capabilities.Capability;
   Control         : out Rose.Capabilities.Capability;
   Held            : out Rose.Capabilities.Capability)
is
begin
   Replaceable_Test_Server.Create_Server
     (Name            => Name,
      Cap_Set_Service => Cap_Set_Service.Constructor_Cap,
      Target          => Target.Held_Call_Cap,
      Served          => Served,
      Control         => Control,
      Held            => Held);
end Rose.Core.Replaceable_Test;
