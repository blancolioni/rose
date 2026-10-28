package Tests.Finalization is

   function Finalization_Test return Boolean;
   --  Controlled types in the userland run-time system (#359): counts of
   --  the calls to Initialize, Adjust and Finalize for objects declared
   --  in a block, copied and assigned, allocated and freed, and left on
   --  the heap when their access type goes out of scope.

end Tests.Finalization;
