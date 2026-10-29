package Tests.Exceptions is

   function Exception_Test return Boolean;
   --  Exception propagation in the userland run-time system (#170): an
   --  exception raised in one subprogram is handled in a caller, with its
   --  name and message; a predefined exception from a failed check; and
   --  an exception that a handler re-raises.

end Tests.Exceptions;
