with Rose.Capabilities;

package Init.Self_Tests is

   function Run
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability;
      Screen          : Rose.Capabilities.Capability;
      Create_Signal   : Rose.Capabilities.Capability;
      Cap_Sets        : Rose.Capabilities.Capability;
      Replace_Process : Rose.Capabilities.Capability)
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
   --  makes signal caps, for its Create_Signal role (#708), Cap_Sets,
   --  the Create cap of the userland cap set server, for its
   --  Cap_Set_Service role (#825), and Replace_Process, mem's Replacer,
   --  for its Replace_Process role (#866).
   --  True if the command completed, False if it could not be run, or
   --  failed because a test failed.

   function Derive_Test
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Derive_Cap      : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability;
      Cap_Sets        : Rose.Capabilities.Capability)
      return Boolean;
   --  Derive capabilities from Find_Cap, the registry's, and check what
   --  rescinding one of them does (ADR 0001, #116): the capability goes on
   --  working, and so do its parent and its siblings, but what was
   --  derived from it does not; and that a capability can be derived from
   --  to a depth of eight, and no more.  If Cap_Sets, the Create cap of
   --  the userland cap set server, isn't null, also check that a parent
   --  passed through a cap set survives its holder's copy (ADR 0032,
   --  #864): deleting the holder's copy leaves its child working, and the
   --  copy taken from the set still revokes the child.  It reports each
   --  check on Standard_Output, and is True if they all passed.

   function Swap_Test
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Derive_Cap      : Rose.Capabilities.Capability)
      return Boolean;
   --  The kernel's part of replacing a running process (ADR 0032, #862).
   --  Launch three swap_probe commands, found through Find_Cap, in a test
   --  bank: probes 1 and 2, which answer their numbers, and probe 3,
   --  which asks probe 1 while probe 1's calls are held, so its call
   --  waits. Swap probes 1 and 2, release the calls, and check that
   --  init's caps to probe 1, and a cap derived from one, reach probe 2;
   --  that probe 3's waiting call was answered by probe 2; and that
   --  init's caps to probe 2 are refused. Destroying the bank ends the
   --  probes. It reports each check on Standard_Output, and is True if
   --  they all passed.

   type Forward_Result is
     (Passed, No_Update_Cap, Nothing_From_Restore, Update_Failed,
      No_Test_Bank, Launch_Failed);
   --  The result of Exec_Forward_Test: Passed, or the step that failed.

   function Exec_Forward_Test
     (Update_Cap    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Old_Launch    : Rose.Capabilities.Capability;
      Install_Cap   : Rose.Capabilities.Capability)
      return Forward_Result;
   --  Install replaceable_test again from Binary_Stream with Update_Cap,
   --  exec's Update, giving it Install_Cap as an install cap, so that it
   --  is a new version and the old one is retired (#902). Then launch it
   --  as a command through Old_Launch, the first version's Launch
   --  interface, against a test bank, which is then destroyed. Passed
   --  if the launch through the retired version's cap works.

   function Hand_Over_Test
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Process_Banks   : Rose.Capabilities.Capability)
      return Boolean;
   --  The store's part of replacing a running process (ADR 0032, #863).
   --  Through Process_Banks, the store's interface for mem, make two
   --  sibling process banks in a test bank, for made-up processes Old and
   --  New, and launch a swap_probe, found through Find_Cap, against a
   --  sub-bank of Old's. Hand Old's bank over to New, and check that the
   --  owners swapped and the probe's charges moved; then end New, as mem
   --  ends the old version after the swap, and check that the probe
   --  still answers. Destroying the test bank ends the probe. It reports
   --  each check on Standard_Output, and is True if they all passed.

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

   function Replace_Held_File
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Cap_Sets        : Rose.Capabilities.Capability;
      Replace_Process : Rose.Capabilities.Capability)
      return Boolean;
   --  Launch the hold_file command, found through Find_Cap, with
   --  Create_Endpoint, Standard_Output, Delete_Cap, Commands and Cap_Sets
   --  for its roles. It launches a file, and holds a writer and a reader
   --  of it. Then replace the file's process with a new version, through
   --  Replace_Process, mem's Replacer (#867, ADR 0032). Hold_file is
   --  persistent, so the checkpoint saves it and the new version of the
   --  file, and the first time it is restored, it checks the file and its
   --  streams. True if the replacement committed.

end Init.Self_Tests;
