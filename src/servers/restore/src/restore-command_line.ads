--  The kernel's command line, from GRUB's multiboot2 line, which the
--  kernel gives restore-install as the boot loader's tag 1 (#836).

package Restore.Command_Line is

   function Has_Argument (Name : String) return Boolean;
   --  True if Name is one of the command line's words.  False if there
   --  is no command line, or no cap to read it, as in restore.

end Restore.Command_Line;
