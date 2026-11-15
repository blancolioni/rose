with Rose.Words;

with Rose.Interfaces.Processes.Process.Client;
with Rose.Interfaces.Storage.Region.Client;
with Rose.Interfaces.Storage.Bank.Client;

package Elf.Loader is

   procedure Load_Elf_Image
     (Process : Rose.Interfaces.Processes.Process.Client.Process_Client;
      Bank    : Rose.Interfaces.Storage.Bank.Client.Bank_Client;
      Image   : Rose.Interfaces.Storage.Region.Client.Region_Client;
      Start   : out Rose.Words.Word;
      Success : out Boolean);

end Elf.Loader;
