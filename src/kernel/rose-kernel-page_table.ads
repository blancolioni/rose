with System;

package Rose.Kernel.Page_Table is

   type Change_Count is mod 2 ** 64;

   function Changes return Change_Count;
   --  How many times a page table has been changed in a way that can take
   --  a page away from user mode: an entry overwritten or cleared, or a
   --  page directory set up or freed. Rose.Kernel.Processes relies on it
   --  to know that a checked invocation record is still mapped (#717), so
   --  any new code here that clears or replaces entries must count itself.

   procedure Init_Page_Table;
   --  Bring the page table to a known state: nothing mapped except
   --  the kernel starting at 16#F000_0000#

   procedure Init_User_Page_Directory
     (Directory_Page : Rose.Addresses.Virtual_Page_Address);
   --  Init_User_Page_Directory is called while Kernel is mapped,
   --  thus the virtual page address argument

   procedure Free_Process_Page_Tables
     (Directory_Page : Rose.Addresses.Virtual_Page_Address);
   --  Give the page tables that a process's page directory has of its
   --  own back to the kernel heap, and clear the entries that held them.
   --  That is every entry not shared with the kernel's directory: the
   --  user range, and the process's mappings above it, such as its stack.
   --  The pages they mapped are not the kernel's, and are not freed.

   procedure Activate_Page_Directory
     (Directory_Page : Rose.Addresses.Virtual_Page_Address);

   procedure Map_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address;
      Physical_Page  : Rose.Addresses.Physical_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      User           : Boolean;
      Managed        : Boolean := False;
      Write_Tracked  : Boolean := False);
   --  Managed is true for a page the memory server maps.  Only those are
   --  part of a checkpoint, so Disable_Page_Writes leaves every other
   --  page writable: device pages such as the VGA text page, and shared
   --  invocation buffers.
   --
   --  Write_Tracked is true for a clean page of a writable segment, which
   --  the memory server maps read-only to see its first write. During a
   --  checkpoint, the kernel makes it writable itself (ADR 0030, section
   --  3).

   procedure Unmap_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address);

   function Is_User_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
      return Boolean;
   --  True if Virtual_Page is present, and a process can reach it from
   --  user mode.

   function Is_Writable_User_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
      return Boolean;
   --  True if Virtual_Page is present, and a process can write it from
   --  user mode. A checkpoint makes a page read-only until it ends.

   function Mapped_Physical_Page
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
      return Rose.Addresses.Physical_Page_Address;
   --  The frame that Virtual_Page is mapped to, or 0 if it is not present
   --  (#768).

   procedure Map_Kernel_Page
     (Virtual_Page   : Rose.Addresses.Virtual_Page_Address;
      Physical_Page  : Rose.Addresses.Physical_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      User           : Boolean);

   procedure Unmap_Kernel_Page
     (Virtual_Page   : Rose.Addresses.Virtual_Page_Address);

   function Kernel_Page_Directory
     return Rose.Addresses.Physical_Address;

   procedure Disable_Page_Writes
     (Directory_Page : Rose.Addresses.Virtual_Page_Address);
   --  Scan all pages referenced by directory pages referenced
   --  by this page.  If write enabled, and mapped by the memory server,
   --  disable and set the checkpoint-read-only flag.

   procedure End_Page_Writes
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Saved          : Boolean);
   --  Clear the checkpoint-read-only flag of every page that still has
   --  it. The page stays read-only, so the memory server sees its next
   --  write. If Saved, the memory server has saved it and it is clean,
   --  so it becomes Write_Tracked (ADR 0030, section 5).

   type Checkpoint_Page_Kind is
     (Other_Page, Checkpoint_Read_Only_Page, Write_Tracked_Page);

   function Checkpoint_Kind
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address)
      return Checkpoint_Page_Kind;
   --  Whether Virtual_Page is a present, read-only user page that is
   --  checkpoint-read-only or Write_Tracked.

   procedure Make_Written
     (Directory_Page : Rose.Addresses.Virtual_Page_Address;
      Virtual_Page   : Rose.Addresses.Virtual_Page_Address);
   --  Make Virtual_Page writable, and clear its checkpoint-read-only and
   --  Write_Tracked flags.

   procedure Copy_Frame
     (From : Rose.Addresses.Physical_Page_Address;
      To   : Rose.Addresses.Physical_Page_Address);
   --  Copy the contents of frame From to frame To, through two windows in
   --  the kernel's address space.

   procedure Copy_Frame
     (From : Rose.Addresses.Physical_Page_Address;
      To   : System.Address);
   --  Copy the contents of frame From to the page at To, which is mapped
   --  writable in the current address space.

   procedure Report_Mapped_Pages
     (Directory_Page : Rose.Addresses.Physical_Address);

end Rose.Kernel.Page_Table;
