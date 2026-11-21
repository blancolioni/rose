with Directory_Server;

procedure Rose.Core.Directory
  (Cap_Set_Service  : in out
     Rose.Interfaces.Caps.Bank_Constructor
       .Bank_Constructor_Object'Class;
   File_Launch      : in out Rose.Interfaces.Processes.Launch
     .Launch_Object'Class;
   Directory_Launch : in out Rose.Interfaces.Processes.Launch
     .Launch_Object'Class;
   Served           : out Rose.Capabilities.Capability;
   Control          : out Rose.Capabilities.Capability)
is
begin
   Directory_Server.Create_Server
     (File_Launch      => File_Launch.Launch_Cap,
      Directory_Launch => Directory_Launch.Launch_Cap,
      Cap_Set_Service  => Cap_Set_Service.Bank_Constructor_Cap,
      Served           => Served,
      Control          => Control);
end Rose.Core.Directory;
