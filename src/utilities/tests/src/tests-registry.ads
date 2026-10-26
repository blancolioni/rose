with Rose.Capabilities;

with Rose.Interfaces.Launch.Client;

package Tests.Registry is

   Create_Endpoint_Index : constant := 8;
   Find_Index            : constant := 10;
   --  Caps that init gives the tests: a create endpoint cap, and the
   --  command registry's Find.

   function Argument_Cap
     (Index : Positive)
      return Rose.Capabilities.Capability;
   --  A copy of cap Index of the caps that init gives the tests.

   function Launcher
     (Name : String)
      return Rose.Interfaces.Launch.Client.Launch_Client;
   --  The Launch interface of the program Name, from the command
   --  registry whose Find init gives the tests.

end Tests.Registry;
