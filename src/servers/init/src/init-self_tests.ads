with Rose.Capabilities;

package Init.Self_Tests is

   function Run
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability)
      return Boolean;
   --  Run the run_self_tests command, found through the command
   --  registry's Find_Cap, and wait for it to finish (#460). It is called
   --  with the caps of its roles (src/utilities/run_self_tests/
   --  rose-core-run_self_tests.idl): Standard_Output, Create_Endpoint,
   --  and Commands, the registry's Commands interface. True if the
   --  command completed, False if it could not be run, or failed
   --  because a test failed.

end Init.Self_Tests;
