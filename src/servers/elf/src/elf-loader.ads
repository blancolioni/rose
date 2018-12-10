with Rose.Objects;

with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Storage.Client;

package Elf.Loader is

   procedure Load_Elf_Image
     (Process : Rose.Objects.Object_Id;
      Store   : Rose.Interfaces.Storage.Client.Storage_Client;
      Image   : Rose.Interfaces.Region.Client.Region_Client;
      Success : out Boolean);

end Elf.Loader;
