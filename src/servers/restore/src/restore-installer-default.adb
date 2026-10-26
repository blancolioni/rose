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
      Process ("command", "drivers", Launch, Default_Flags);
      Process ("keyboard", "drivers", Launch,
               (Provides_Keyboard_Interface => True, others => False));
      Process ("event-input-stream", "drivers", Launch,
               (Requires_Keyboard_Interface => True,
                Provides_Input_Stream       => True,
                others                      => False));

      --  Saved, not launched: each file and directory is its own
      --  process, launched when it is created (ADR 0003).

      Process ("file", "drivers", Save, Default_Flags);
      Process ("directory", "drivers", Save, Default_Flags);
      Process ("echo", "bin", Save, Default_Flags);
      Process ("tests", "bin", Save, Default_Flags);
      Process ("petal", "bin", Save,
               (Requires_Input_Stream => True,
                others => False));
   end Iterate_Steps;

end Restore.Installer.Default;
