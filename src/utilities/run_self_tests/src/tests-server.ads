package Tests.Server is

   function Instance_Test return Boolean;
   --  The table of an instanced server in user programs (#500): the caps
   --  of three thousand objects, two endpoints each, well past the 1024
   --  entries that a boot module's table holds, all found again; the
   --  first cap set for an endpoint is the one found; and objects removed
   --  from a second table are gone while the others stay.

end Tests.Server;
