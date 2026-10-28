with Rose.Interfaces.Block_Device.Client;

package Restore.Installer is

   type Install_Action is (Launch, Save);

   type Install_Flag is
     (Provides_Keyboard_Interface,
      Requires_Keyboard_Interface,
      Provides_Input_Stream,
      Requires_Input_Stream,
      Provides_Console,
      Is_Driver);
   --  Provides_Console: the program is the persistent console, which
   --  init gives everything installed after it as standard output.
   --  Is_Driver: the program is from the drivers directory, so init may
   --  give it more caps than other programs (#115).  Install sets it.

   type Install_Flag_Array is array (Install_Flag) of Boolean;

   type Step_Iterator is access
     procedure (Process : not null access
                  procedure (Name     : String;
                             Category : String;
                             Action   : Install_Action;
                             Flags : Install_Flag_Array));

   procedure Install
     (To     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Source : Step_Iterator);

end Restore.Installer;
