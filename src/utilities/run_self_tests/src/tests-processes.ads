package Tests.Processes is

   function End_Process_Test return Boolean;
   --  Launch a file, and end its process through the Process interface
   --  that Launch returned (#424). Check that the file's caps then fail
   --  (#320), and that the ended process's cap doesn't reach a process
   --  launched after it.

end Tests.Processes;
