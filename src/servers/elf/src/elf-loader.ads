with Rose.Words;

with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Storage.Client;

package Elf.Loader is

   procedure Load_Elf_Image
     (Process : Rose.Interfaces.Process.Client.Process_Client;
      Store   : Rose.Interfaces.Storage.Client.Storage_Client;
      Image   : Rose.Interfaces.Region.Client.Region_Client;
      Start   : out Rose.Words.Word;
      Success : out Boolean);

end Elf.Loader;
