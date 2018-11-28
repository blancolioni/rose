with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.Block_Device.Client;

package Restore.Installer is

   procedure Install
     (From : Rose.Interfaces.Directory.Client.Directory_Client;
      To   : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

end Restore.Installer;
