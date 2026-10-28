package Init.Run is

   procedure Run_Init (Install : Boolean);
   --  Start the boot servers and the restore server.  If Install is
   --  True, this is the install media's init: it also starts isofs on
   --  hd1, installs a new system if there is no checkpoint, and then
   --  launches the tests and petal.  Otherwise it is the boot disk's
   --  init, which only restores a checkpoint (#373).

end Init.Run;
