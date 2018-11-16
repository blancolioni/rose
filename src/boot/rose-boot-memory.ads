package Rose.Boot.Memory is

   --  Configure memory
   procedure Configure_Memory;

   --  Protect_Modules: make sure that we don't overwrite the
   --  loaded modules during cache initialisation
   procedure Protect_Modules;

end Rose.Boot.Memory;
