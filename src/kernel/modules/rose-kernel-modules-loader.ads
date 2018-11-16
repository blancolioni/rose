package Rose.Kernel.Modules.Loader is

   procedure Load_ELF_Module
     (Pid         : Rose.Objects.Process_Id;
      Base, Bound : Physical_Address) is null;

end Rose.Kernel.Modules.Loader;
