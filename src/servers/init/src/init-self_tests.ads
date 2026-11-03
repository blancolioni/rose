with Rose.Capabilities;

package Init.Self_Tests is

   function Run
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability)
      return Boolean;
   --  Run the run_self_tests command, found through the command
   --  registry's Find_Cap, and wait for it to finish (#460). It is called
   --  with the caps of its roles (src/utilities/run_self_tests/
   --  rose-core-run_self_tests.idl): Standard_Output, Create_Endpoint,
   --  Commands, the registry's Commands interface, and Clock, the
   --  clock's Clock interface. True if the command completed, False if
   --  it could not be run, or failed because a test failed.

   function Derive_Test
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Derive_Cap      : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability;
      Create_Cap_Set  : Rose.Capabilities.Capability)
      return Boolean;
   --  Derive capabilities from Find_Cap, the registry's, and check what
   --  rescinding one of them does (ADR 0001, #116): the capability goes on
   --  working, and so do its parent and its siblings, but what was
   --  derived from it does not; and that a capability can be derived from
   --  to a depth of eight, and no more.  It reports each check on
   --  Standard_Output, and is True if they all passed.  It leaves two
   --  derived capabilities in a cap set, which the caps server holds
   --  (Create_Cap_Set is its constructor), so that a checkpoint has
   --  derivation records that a restored process names.

end Init.Self_Tests;
