with Rose.System_Calls;

package body Tests.Registry is

   Registry : Rose.Interfaces.Commands.Client.Commands_Client;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint return Rose.Capabilities.Capability is
   begin
      return Rose.System_Calls.Create_Endpoint_Capability;
   end Create_Endpoint;

   --------------
   -- Launcher --
   --------------

   function Launcher
     (Name : String)
      return Rose.Interfaces.Launch.Client.Launch_Client
   is
   begin
      return Rose.Interfaces.Commands.Client.Find (Registry, Name);
   end Launcher;

   ------------------
   -- Set_Commands --
   ------------------

   procedure Set_Commands
     (Commands : Rose.Interfaces.Commands.Client.Commands_Client)
   is
   begin
      Registry := Commands;
   end Set_Commands;

end Tests.Registry;
