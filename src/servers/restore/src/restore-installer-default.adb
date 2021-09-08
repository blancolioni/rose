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
      Process ("event-in", "drivers", Launch,
               (Requires_Keyboard_Interface => True,
                Provides_Input_Stream       => True,
                others                      => False));
      Process ("echo", "bin", Save, Default_Flags);
      Process ("petal", "bin", Save);
   end Iterate_Steps;

end Restore.Installer.Default;
