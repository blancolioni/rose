with Rose.Capabilities;

with Rose.Interfaces.Commands;
with Rose.Interfaces.Launch;

package Tests.Registry is

   procedure Set_Commands
     (Commands : in out Rose.Interfaces.Commands.Commands_Object'Class);
   --  Find programs through Commands, the command registry's Commands
   --  interface, which Run_Self_Tests is given for its Find_Command role
   --  (#460).

   function Create_Endpoint return Rose.Capabilities.Capability;
   --  The create endpoint cap that Run_Self_Tests is given for its
   --  Create_Endpoint role.

   function Launcher
     (Name : String)
      return Rose.Interfaces.Launch.Launch_Object'Class;
   --  The Launch interface of the program Name, from the command
   --  registry.

   function Launcher_Cap
     (Name : String)
      return Rose.Capabilities.Capability;
   --  The cap of the Launch interface of the program Name.

   function Launch
     (Name : String;
      Caps : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;
   --  Launch the program Name with Caps, and return the interface that
   --  it publishes.

end Tests.Registry;
