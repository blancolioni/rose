package Rose.Last_Chance is

   --  For a command built for the host (#1101): the exception that a
   --  command raises, as Rose.Command_Line.Command_Failed, to fail with a
   --  message. On Rose, the userland run-time system's last chance handler
   --  turns it into the failure of the launch call; on the host, the main
   --  that idle generates catches it, and Rose.Command_Line.Host.Fail
   --  reports it.

   Failed : exception;

end Rose.Last_Chance;
