package Tests.Dispatching is

   function Dispatching_Test return Boolean;
   --  Tagged types in the userland run-time system (#357): a derived
   --  type, an interface, dispatching calls through class-wide
   --  parameters, and membership tests.

end Tests.Dispatching;
