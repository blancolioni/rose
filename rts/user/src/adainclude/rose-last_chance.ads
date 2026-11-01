package Rose.Last_Chance is

   --  What happens to an exception that nothing handles: the last chance
   --  handler (Ada.Exceptions.Last_Chance_Handler) reports it through the
   --  Failure handler, if there is one, and then exits the process with
   --  status 1.

   pragma Preelaborate;

   type Failure_Handler is access procedure (Message : String);

   procedure Set_Handler (Handler : Failure_Handler);
   --  Report unhandled exceptions to Handler. A command's launch support
   --  (Rose.Command_Line.Launch) uses this to fail the launch invocation,
   --  so that the shell is not left waiting for a reply.

   function Handler return Failure_Handler;

end Rose.Last_Chance;
