package body Restore.Installer.Default is

   -------------------
   -- Iterate_Steps --
   -------------------

   procedure Iterate_Steps
     (Process : not null access
        procedure (Name     : String;
                   Category : String;
                   Action   : Install_Action))
   is
   begin
      Process ("exec", "drivers", Launch);
      Process ("command", "drivers", Launch);
      Process ("keyboard", "drivers", Launch);
      Process ("echo", "bin", Save);
      Process ("petal", "bin", Save);
   end Iterate_Steps;

end Restore.Installer.Default;
