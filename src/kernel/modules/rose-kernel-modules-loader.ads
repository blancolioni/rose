with Rose.Objects;

package Rose.Kernel.Modules.Loader is

   procedure Load_ELF_Module
     (Oid         : Rose.Objects.Object_Id;
      Base, Bound : Physical_Address) is null;

end Rose.Kernel.Modules.Loader;
