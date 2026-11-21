package Tests.Processes is

   function End_Process_Test return Boolean;

   function Process_Limit_Test return Boolean;
   --  Launch probes until a launch fails, as it has to once the system
   --  is out of processes, then end them all, and check that a launch
   --  works again (#915). Scripts/test-boot checks that no probe is left
   --  starting at the next checkpoint, which a failed launch did before.
   --  It is out of the suite for now: in CI, running out of processes
   --  can stop the whole system in mem's New_Process (#915, case 2).

   function Bank_End_Test return Boolean;
   --  A destroyed bank ends its processes (ADR 0015, #750): launch a file
   --  server against a sub-bank of this process's own bank, destroy the
   --  sub-bank, and check that the server's caps then fail. A process
   --  that exec launched always has a bank of its own, so it fails
   --  without one.
   --  Launch a probe, and end its process through the Process_Control
   --  cap that Launch returned (#424). Check that the cap answers only
   --  Process_Control's operations, and not the probe's own Process
   --  ones (#99), that the probe's caps fail once it has ended (#320),
   --  and that the ended process's cap doesn't reach a process launched
   --  after it.

end Tests.Processes;
