with Rose.Addresses;
with Rose.Objects;

package Mem.Shared_Pages is

   --  Physical pages of shared segments, by the store page they hold
   --  (ADR 0025). Every process that maps a store page shared maps the
   --  same physical page, so they all see its writes. Each page counts its
   --  mappings, and records whether it has been written since it was read
   --  from its region. A shared page is never evicted.

   function Find
     (Page : Rose.Objects.Page_Object_Id)
      return Rose.Addresses.Physical_Page_Address;
   --  The physical page that holds the store page Page, or zero if none
   --  does.

   function Is_Shared
     (Physical : Rose.Addresses.Physical_Page_Address)
      return Boolean;

   procedure Add
     (Page     : Rose.Objects.Page_Object_Id;
      Physical : Rose.Addresses.Physical_Page_Address;
      Success  : out Boolean);
   --  Record that Physical holds the store page Page, and has just been
   --  mapped once. Success is False if the table is full.

   procedure Acquire (Physical : Rose.Addresses.Physical_Page_Address);
   --  Count one more mapping of the shared page Physical.

   procedure Set_Modified (Physical : Rose.Addresses.Physical_Page_Address);
   --  Physical has been written, so the next checkpoint saves it. Nothing
   --  happens if it isn't shared.

   procedure Release
     (Physical : Rose.Addresses.Physical_Page_Address;
      Last     : out Boolean;
      Modified : out Boolean);
   --  Count one mapping fewer of the shared page Physical. If that was its
   --  last, Last is True, Modified is True if it was written, and the page
   --  is no longer in the table: the caller writes it back to its region
   --  if it was modified, and frees it. The next checkpoint lists it as
   --  released.

   procedure Restore
     (Page     : Rose.Objects.Page_Object_Id;
      Physical : Rose.Addresses.Physical_Page_Address;
      Success  : out Boolean);
   --  Record that Physical holds the store page Page, restored from a
   --  checkpoint. Nothing maps it yet. It is modified, since its region
   --  doesn't have its contents, and the next checkpoint saves it again.
   --  Success is False if the table is full or already has Page.

   --  A checkpoint (Mem.Checkpoints) lists the released pages and the
   --  written ones, then saves the written ones, then clears both lists.

   function Has_Checkpoint_Pages return Boolean;
   --  True if a page has been written or released since the last
   --  checkpoint.

   procedure Iterate_Released
     (Action : not null access
        procedure (Page : Rose.Objects.Page_Object_Id));
   --  Call Action for each page released since the last checkpoint.

   procedure Iterate_Unsaved
     (Action : not null access
        procedure (Page     : Rose.Objects.Page_Object_Id;
                   Physical : Rose.Addresses.Physical_Page_Address));
   --  Call Action for each page written since the last checkpoint.

   procedure Clear_Checkpoint_Pages;
   --  The checkpoint has saved the pages and listed the released ones.

end Mem.Shared_Pages;
