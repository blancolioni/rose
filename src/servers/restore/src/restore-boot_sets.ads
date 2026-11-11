--  The update boot's boot sets (#828, ADR 0031).  hd0's boot partition
--  has two slots, /boot/a.tar and /boot/b.tar, each with a boot set, and
--  a menu, /boot/grub/grub.cfg, whose first line names the current slot
--  and its version (scripts/make-boot-disk).  An update writes the
--  install media's boot set to the other slot, and then the menu.  Both
--  are written in place: each file is contiguous, and starts on a block
--  of the partition, so neither the FAT nor a directory changes.

with Rose.Interfaces.Block_Device;

package Restore.Boot_Sets is

   type Boot_Partition is private;

   procedure Find
     (Partition : out Boot_Partition;
      Found     : out Boolean);
   --  Find hd0's boot partition's menu and slots, and which slot is
   --  current.  Found is False, after saying why, if there is no boot
   --  partition, or it doesn't have two slots, as on a disk installed
   --  before #828.  Nothing is written.

   procedure Write_Update
     (Partition : Boot_Partition;
      Written   : out Boolean);
   --  Write the install media's boot set to the slot that isn't current,
   --  and then the menu, which makes that slot current and has an entry
   --  for the set it replaces.  Written is False, after saying why, if
   --  either can't be written.  Until the menu is written, the old set
   --  is current.

private

   type Slot_Name is (Slot_A, Slot_B);

   type File_Extent is record
      First : Rose.Interfaces.Block_Device.Block_Address_Type := 0;
      Count : Rose.Interfaces.Block_Device.Block_Address_Type := 0;
   end record;
   --  A file's blocks on the boot partition

   type Slot_Extents is array (Slot_Name) of File_Extent;

   Max_Version_Length : constant := 32;

   type Boot_Partition is record
      Menu         : File_Extent;
      Slots        : Slot_Extents;
      Current      : Slot_Name := Slot_A;
      Version      : String (1 .. Max_Version_Length) := (others => ' ');
      Version_Last : Natural := 0;
      --  The current set's version, from the menu's first line
   end record;

end Restore.Boot_Sets;
