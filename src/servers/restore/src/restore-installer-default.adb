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

      --  The persistent console is the terminal (#454): it draws what it
      --  is written, and its line discipline reads the keyboard. It comes
      --  straight after the keyboard, so that everything installed after
      --  it writes to it.

      Process ("vga-console", "drivers", Launch,
               (Requires_Keyboard_Interface => True,
                Provides_Console            => True,
                Provides_Input_Stream       => True,
                others                      => False));
      Process ("command", "drivers", Launch, Default_Flags);

      --  The clock is persistent, so that a restore carries its time on
      --  (ADR 0020).

      Process ("clock", "drivers", Launch,
               (Provides_Clock => True, others => False));

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
      Process ("petal", "bin", Save,
               (Requires_Input_Stream => True,
                others => False));
   end Iterate_Steps;

end Restore.Installer.Default;
