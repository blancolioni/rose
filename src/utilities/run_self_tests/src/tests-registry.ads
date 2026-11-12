with Rose.Capabilities;

with Rose.Interfaces.Commands;
with Rose.Interfaces.Launch;

package Tests.Registry is

   procedure Set_Commands
     (Commands : in out Rose.Interfaces.Commands.Commands_Object'Class);
   --  Find programs through Commands, the command registry's Commands
   --  interface, which Run_Self_Tests is given for its Find_Command role
   --  (#460).

   procedure Set_Clock (Clock : Rose.Capabilities.Capability);
   --  The clock's Clock interface, which Run_Self_Tests is given for its
   --  Clock_Service role (ADR 0020).

   function Clock return Rose.Capabilities.Capability;
   --  The clock's Clock interface.

   procedure Set_Bank (Bank : Rose.Capabilities.Capability);
   --  The Bank that Run_Self_Tests is given for its Storage_Service role,
   --  or the null cap if it has none, as when petal runs it.

   function Bank return Rose.Capabilities.Capability;
   --  The bank, or the null cap.

   procedure Set_Screen (Screen : Rose.Capabilities.Capability);
   --  The screen driver's Surface, which Run_Self_Tests is given for its
   --  Screen_Service role when there is a framebuffer (#582), or the null
   --  cap.

   function Screen return Rose.Capabilities.Capability;
   --  The screen's surface, or the null cap.

   procedure Set_Cap_Sets (Cap_Sets : Rose.Capabilities.Capability);
   --  The userland cap set server's Constructor, which Run_Self_Tests is
   --  given for its Cap_Set_Service role (#825), or the null cap.

   function Cap_Sets return Rose.Capabilities.Capability;
   --  The cap set server's Constructor, or the null cap.

   procedure Set_Replacer (Replacer : Rose.Capabilities.Capability);
   --  Mem's Replacer, which Run_Self_Tests is given for its
   --  Replace_Process role on a fresh install (#866), or the null cap.

   function Replacer return Rose.Capabilities.Capability;
   --  Mem's Replacer, or the null cap.

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
   --  it publishes, or the null cap if the launch failed.

end Tests.Registry;
