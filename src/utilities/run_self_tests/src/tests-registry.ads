with Rose.Capabilities;

with Rose.Interfaces.Commands.Client;
with Rose.Interfaces.Launch.Client;

package Tests.Registry is

   procedure Set_Commands
     (Commands : Rose.Interfaces.Commands.Client.Commands_Client);
   --  Find programs through Commands, the command registry's Commands
   --  interface, which Run_Self_Tests is given for its Find_Command role
   --  (#460).

   function Create_Endpoint return Rose.Capabilities.Capability;
   --  The create endpoint cap that Run_Self_Tests is given for its
   --  Create_Endpoint role.

   function Launcher
     (Name : String)
      return Rose.Interfaces.Launch.Client.Launch_Client;
   --  The Launch interface of the program Name, from the command
   --  registry.

end Tests.Registry;
