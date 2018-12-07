with Rose.Interfaces.Storage.Client;
with Rose.Interfaces.Stream_Reader.Client;

package Exec.Library is

   function Install
     (Storage   : Rose.Interfaces.Storage.Client.Storage_Client;
      ELF_Image : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client)
     return Rose.Capabilities.Capability;

end Exec.Library;
