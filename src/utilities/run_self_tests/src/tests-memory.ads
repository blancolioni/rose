package Tests.Memory is

   function Copy_Test return Boolean;
   --  memcpy copies exactly the bytes it is asked to (#455): the
   --  userland run-time system's copied one more, past the end of the
   --  destination.

end Tests.Memory;
