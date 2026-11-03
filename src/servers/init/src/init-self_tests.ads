with Rose.Capabilities;

package Init.Self_Tests is

   function Run
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability;
      Storage         : Rose.Capabilities.Capability)
      return Boolean;
   --  Run the run_self_tests command, found through the command
   --  registry's Find_Cap, and wait for it to finish (#460). It is called
   --  with the caps of its roles (src/utilities/run_self_tests/
   --  rose-core-run_self_tests.idl): Standard_Output, Create_Endpoint,
   --  Commands, the registry's Commands interface, Clock, the clock's
   --  Clock interface, and a sub-bank of the root bank of Storage, the
   --  storage server's Storage interface, for its Storage_Service role.
   --  True if the command completed, False if it could not be run, or
   --  failed because a test failed.

   function Derive_Test
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Derive_Cap      : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability)
      return Boolean;
   --  Derive capabilities from Find_Cap, the registry's, and check what
   --  rescinding one of them does (ADR 0001, #116): the capability goes on
   --  working, and so do its parent and its siblings, but what was
   --  derived from it does not; and that a capability can be derived from
   --  to a depth of eight, and no more.  It reports each check on
   --  Standard_Output, and is True if they all passed.

end Init.Self_Tests;
