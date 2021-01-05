with Rose.Interfaces.Block_Device.Client;

package Restore.Installer is

   type Install_Action is (Launch, Save);

   type Step_Iterator is access
     procedure (Process : not null access
                  procedure (Name     : String;
                             Category : String;
                             Action   : Install_Action));

   procedure Install
     (To     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Source : Step_Iterator);

end Restore.Installer;
