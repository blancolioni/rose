--  Writing GRUB to the system disk, so that Rose boots from it (#322).
--  The build makes the three files in /rose/install/boot on the install
--  media (scripts/make-boot-disk).

package Restore.Boot_Disk is

   procedure Write_Boot_Loader;
   --  Write boot.img to the MBR, keeping the rest of block 0, which holds
   --  the protective MBR's partition entry and signature; core.img to the
   --  BIOS boot partition; and boot.fat to the boot partition.  A file
   --  that is missing, or doesn't fit, is reported and left out.

end Restore.Boot_Disk;
