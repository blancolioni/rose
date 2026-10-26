with Rose.Capabilities.Standard;
with Rose.Invocation;
with Rose.System_Calls.Client;

with Rose.Interfaces.Cap_Set;
with Rose.Interfaces.Map.Client;

package body Tests.Registry is

   Find_Index : constant := 10;
   --  The command registry's Find, in the caps init gives the tests.

   function Find_Cap return Rose.Capabilities.Capability;

   --------------
   -- Find_Cap --
   --------------

   function Find_Cap return Rose.Capabilities.Capability is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send
        (Params, Rose.Capabilities.Standard.Argument_Cap_Set);
      Rose.System_Calls.Receive_Caps
        (Params, Rose.Interfaces.Cap_Set.Subprogram_Count);
      Rose.System_Calls.Invoke_Capability (Params);
      return Rose.System_Calls.Client.Get_Capability
        (Params.Caps (Rose.Interfaces.Cap_Set.Get_Cap_Index),
         (1 => Find_Index));
   end Find_Cap;

   --------------
   -- Launcher --
   --------------

   function Launcher
     (Name : String)
      return Rose.Interfaces.Launch.Client.Launch_Client
   is
      Registry : Rose.Interfaces.Map.Client.Map_Client;
   begin
      Rose.Interfaces.Map.Client.Open_Cap_Set
        (Registry,
         Add    => Rose.Capabilities.Null_Capability,
         Remove => Rose.Capabilities.Null_Capability,
         Find   => Find_Cap);
      return Rose.Interfaces.Map.Client.Find (Registry, Name);
   end Launcher;

end Tests.Registry;
