package Rose.Kernel.Modules.Init is

   --  boot-time initialisation of modules
   --  modules that have to be launched while the
   --  system is still booting, and therefore cannot
   --  use the standard process creation mechanisms.

   procedure Load_Modules;

end Rose.Kernel.Modules.Init;
