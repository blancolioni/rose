package body Restore.Installer.Default is

   -------------------
   -- Iterate_Steps --
   -------------------

   procedure Iterate_Steps
     (Process : not null access
        procedure (Name     : String;
                   Category : String;
                   Action   : Install_Action;
                   Flags    : Install_Flag_Array))
   is
      Default_Flags : constant Install_Flag_Array := (others => False);
   begin
      Process ("exec", "drivers", Launch, Default_Flags);

      --  The keyboard comes first, because the console reads it. It
      --  writes nothing, so it doesn't need the persistent console.

      Process ("keyboard", "drivers", Launch,
               (Provides_Keyboard_Interface => True, others => False));

      --  With a framebuffer, the screen driver holds it, and the keys go
      --  to the screen (ADR 0022, #582, #653).

      Process ("screen", "drivers", Launch,
               (Requires_Framebuffer        => True,
                Requires_Keyboard_Interface => True,
                Provides_Screen             => True,
                others                      => False));

      --  The persistent console is the terminal (#454): it draws what it
      --  is written, and its line discipline reads the keyboard. It comes
      --  straight after the keyboard and the screen, so that everything
      --  installed after it writes to it. With a framebuffer it is on the
      --  screen's surface, and takes its keys from the surface's events
      --  (#653); without one, it is on the VGA text page, and reads the
      --  keyboard itself.

      Process ("surface-console", "drivers", Launch,
               (Requires_Framebuffer  => True,
                Requires_Screen       => True,
                Provides_Console      => True,
                Provides_Input_Stream => True,
                others                => False));
      Process ("vga-console", "drivers", Launch,
               (Requires_No_Framebuffer     => True,
                Requires_Keyboard_Interface => True,
                Provides_Console            => True,
                Provides_Input_Stream       => True,
                others                      => False));
      Process ("command", "drivers", Launch, Default_Flags);

      --  The clock is persistent, so that a restore carries its time on
      --  (ADR 0020).

      Process ("clock", "drivers", Launch,
               (Provides_Clock => True, others => False));

      --  The cap sets of persistent processes, which a restore keeps
      --  (#825). It writes to the console when it is restored.

      Process ("caps", "drivers", Launch,
               (Provides_Cap_Sets => True,
                Requires_Console  => True,
                others            => False));

      --  Saved, not launched: each file and directory is its own
      --  process, launched when it is created (ADR 0003).

      Process ("file", "drivers", Save, Default_Flags);
      Process ("directory", "drivers", Save, Default_Flags);
      Process ("echo", "bin", Save, Default_Flags);
      Process ("list", "bin", Save, Default_Flags);
      Process ("cat", "bin", Save, Default_Flags);
      Process ("touch", "bin", Save, Default_Flags);
      Process ("mkdir", "bin", Save, Default_Flags);
      Process ("rename", "bin", Save, Default_Flags);
      Process ("remove", "bin", Save, Default_Flags);
      Process ("rmdir", "bin", Save, Default_Flags);
      Process ("copy", "bin", Save, Default_Flags);
      Process ("clear", "bin", Save, Default_Flags);
      Process ("date", "bin", Save, Default_Flags);
      Process ("set_date", "bin", Save, Default_Flags);
      Process ("uptime", "bin", Save, Default_Flags);
      Process ("user", "bin", Save, Default_Flags);
      Process ("auth", "bin", Save, Default_Flags);
      Process ("rights", "bin", Save, Default_Flags);
      Process ("login", "bin", Save, Default_Flags);
      Process ("run_self_tests", "bin", Save, Default_Flags);
      Process ("hold_derived", "bin", Save, Default_Flags);
      Process ("hold_file", "bin", Save, Default_Flags);
      Process ("hold_home", "bin", Save, Default_Flags);
      Process ("swap_probe", "bin", Save, Default_Flags);

      --  The test server of the Replaceable interface, which
      --  run_self_tests launches (#865).

      Process ("replaceable_test", "bin", Save, Default_Flags);
      Process ("argv", "bin", Save, Default_Flags);
      Process ("keytest", "bin", Save, Default_Flags);
      Process ("kilo", "bin", Save, Default_Flags);
      Process ("petal", "bin", Save,
               (Requires_Input_Stream => True,
                others => False));
   end Iterate_Steps;

end Restore.Installer.Default;
