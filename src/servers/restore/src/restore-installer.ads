with Rose.Interfaces.File_System.Client;

package Restore.Installer is

   type Install_Action is (Launch, Save);

   type Install_Flag is
     (Provides_Keyboard_Interface,
      Requires_Keyboard_Interface,
      Provides_Input_Stream,
      Requires_Input_Stream,
      Provides_Console,
      Is_Driver,
      Provides_Clock,
      Requires_Framebuffer,
      Provides_Screen,
      Requires_No_Framebuffer,
      Requires_Screen);
   --  Provides_Console: the program is the persistent console, a
   --  Terminal, whose Stream_Writer init gives everything installed after
   --  it as standard output (#454). With Provides_Input_Stream as well,
   --  its Reader is standard input.
   --  Is_Driver: the program is from the drivers directory, so init may
   --  give it more caps than other programs (#115).  Install sets it.
   --  Provides_Clock: the program is the clock (ADR 0020), a Clock_Admin
   --  whose ancestor, Clock, init gets. Is_Driver stays where it is in
   --  the flag word, because init reads that bit.
   --  Requires_Framebuffer: init installs the program only if the boot
   --  loader gave a framebuffer, and otherwise replies with no caps; init
   --  reads this bit too. Provides_Screen: the program is the screen
   --  driver, whose surface init gets (#582). Requires_No_Framebuffer:
   --  init installs the program only if there is no framebuffer, as for
   --  vga-console; init reads this bit too. Requires_Screen: the program
   --  is given the screen driver's surface after its own caps, as the
   --  console on the surface is (#653). A program has at most one of
   --  Requires_Keyboard_Interface and Requires_Screen.

   type Install_Flag_Array is array (Install_Flag) of Boolean;

   type Step_Iterator is access
     procedure (Process : not null access
                  procedure (Name     : String;
                             Category : String;
                             Action   : Install_Action;
                             Flags : Install_Flag_Array));

   procedure Install
     (File_System : Rose.Interfaces.File_System.Client.File_System_Client;
      Source      : Step_Iterator);
   --  Install the programs Source gives from /rose/install in
   --  File_System.

end Restore.Installer;
