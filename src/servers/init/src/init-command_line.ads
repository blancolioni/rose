--  The kernel's command line, from GRUB's multiboot2 line, which the boot
--  loader gives as its tag 1 (#836, #1009).

package Init.Command_Line is

   function Has_Argument (Name : String) return Boolean;
   --  True if Name is one of the command line's words. False if there is
   --  no command line, or the kernel refuses the tag.

end Init.Command_Line;
