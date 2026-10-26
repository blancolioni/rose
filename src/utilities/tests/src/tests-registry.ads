with Rose.Interfaces.Launch.Client;

package Tests.Registry is

   function Launcher
     (Name : String)
      return Rose.Interfaces.Launch.Client.Launch_Client;
   --  The Launch interface of the program Name, from the command
   --  registry whose Find init gives the tests.

end Tests.Registry;
