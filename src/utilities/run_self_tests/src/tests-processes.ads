package Tests.Processes is

   function End_Process_Test return Boolean;

   function Bank_End_Test return Boolean;
   --  A destroyed bank ends its processes (ADR 0015, #750): launch a file
   --  server against a sub-bank of this process's own bank, destroy the
   --  sub-bank, and check that the server's caps then fail. A process
   --  that exec launched always has a bank of its own, so it fails
   --  without one.
   --  Launch a file, and end its process through the Process interface
   --  that Launch returned (#424). Check that the file's caps then fail
   --  (#320), and that the ended process's cap doesn't reach a process
   --  launched after it.

end Tests.Processes;
