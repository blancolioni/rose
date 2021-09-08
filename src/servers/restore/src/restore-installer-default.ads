package Restore.Installer.Default is

   procedure Iterate_Steps
     (Process : not null access
        procedure (Name     : String;
                   Category : String;
                   Action   : Install_Action;
                   Flags : Install_Flag_Array));

end Restore.Installer.Default;
