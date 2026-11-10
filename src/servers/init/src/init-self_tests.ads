with Rose.Capabilities;

package Init.Self_Tests is

   function Run
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability;
      Screen          : Rose.Capabilities.Capability;
      Create_Signal   : Rose.Capabilities.Capability)
      return Boolean;
   --  Run the run_self_tests command, found through the command
   --  registry's Find_Cap, and wait for it to finish (#460). It is called
   --  with the caps of its roles (src/utilities/run_self_tests/
   --  rose-core-run_self_tests.idl): Standard_Output, Create_Endpoint,
   --  Commands, the registry's Commands interface, Clock, the clock's
   --  Clock interface, a sub-bank of the system bank (Init.Grants), for
   --  its Storage_Service role,
   --  Screen, the screen driver's surface or the null cap, for its
   --  Screen_Service role (#582), and Create_Signal, the kernel cap that
   --  makes signal caps, for its Create_Signal role (#708).
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

   function Start_Derived_Holder
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Derive_Cap      : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability)
      return Boolean;
   --  Derive Parent from Commands, the registry's Commands interface,
   --  and Child from Parent, and give them to the hold_derived command,
   --  found through Find_Cap, with Create_Endpoint, Standard_Output and
   --  Rescind_Cap for its roles (#691).  Hold_derived is persistent, so
   --  the checkpoint saves a process that names derivation records, and
   --  the first time it is restored, it checks that Child still works
   --  and is revoked when it rescinds Parent.  True if it was launched.

end Init.Self_Tests;
