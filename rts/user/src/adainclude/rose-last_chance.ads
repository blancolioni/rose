package Rose.Last_Chance is

   --  What happens to an exception that nothing handles: the last chance
   --  handler (Ada.Exceptions.Last_Chance_Handler) reports it through the
   --  Failure handler, if there is one, and then exits the process with
   --  status 1.

   pragma Preelaborate;

   Failed : exception;
   --  Raised with a message, this fails the program with that message
   --  alone: the last chance handler gives the Failure handler the
   --  message, without "unhandled exception" and the exception's name. A
   --  command raises it, as Rose.Command_Line.Command_Failed, to fail its
   --  launch call with a message for the user (#802). Any other exception
   --  is a bug, and is reported with its name.

   type Failure_Handler is access procedure (Message : String);

   procedure Set_Handler (Handler : Failure_Handler);
   --  Report unhandled exceptions to Handler. A command's launch support
   --  (Rose.Command_Line.Launch) uses this to fail the launch invocation,
   --  so that the shell is not left waiting for a reply.

   function Handler return Failure_Handler;

end Rose.Last_Chance;
