with Rose.Interfaces.Files.File_System.Client;

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
      Requires_Screen,
      Provides_Cap_Sets,
      Requires_Console,
      Test_Only,
      Requires_Input_Controller);
   --  Provides_Console: the program is the persistent console, a
   --  Terminal, whose Writer init gives everything installed after
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
   --  Provides_Cap_Sets: the program is the userland cap set server
   --  (#825), whose Constructor init gets. Requires_Console: the program
   --  is given the persistent console's Writer after its own caps, in
   --  place of the keyboard or the surface, so that it can write to the
   --  console; it has neither of the other two.
   --  Test_Only: the program is only for the tests, and is installed, or
   --  updated, only by a test install, with self-tests on the kernel's
   --  command line (#1009). It comes after init's bits, so they don't
   --  move.
   --  Requires_Input_Controller: the program is given the ps2 driver's
   --  Input_Controller after its own caps, the keyboard and the mouse in
   --  one cap, as the screen driver is (#588). Provides_Keyboard_Interface
   --  is the program that publishes it; the others that read the
   --  keyboard are given its Event_Source ancestor. A program has at most
   --  one of the Requires_ flags that give a cap.
   --  In the install list, /rose/install/list (#1100), a flag is named
   --  by its name in lower case, apart from Is_Driver, which the
   --  category sets. scripts/check-install-list reads the names from
   --  here.

   type Install_Flag_Array is array (Install_Flag) of Boolean;

   procedure Install
     (File_System : Rose.Interfaces.Files.File_System.Client
       .File_System_Client);
   --  Install the programs that the install list, /rose/install/list
   --  in File_System, names (#1100), in its order, from /rose/install.
   --  A line of the list that can't be read is refused, and the console
   --  says why.

   procedure Update
     (File_System : Rose.Interfaces.Files.File_System.Client
       .File_System_Client);
   --  The update boot (ADR 0031, section 4; #829): install the saved
   --  programs of the install list in File_System again, in
   --  the restored exec, with Exec.Update, and replace their entries in
   --  the restored command registry. The restored processes must be
   --  running. Launched programs are persistent, and are left as they
   --  are. File and directory are installed again too: every directory
   --  process holds their launch caps, which launch the newest version
   --  once the old one is retired (#902). Then each restored server that
   --  implements Replaceable is replaced with the new version of its
   --  program (Restore.Replacing, #830).

end Restore.Installer;
