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
   --  Physical has been written. Nothing happens if it isn't shared.

   procedure Release
     (Physical : Rose.Addresses.Physical_Page_Address;
      Last     : out Boolean;
      Modified : out Boolean);
   --  Count one mapping fewer of the shared page Physical. If that was its
   --  last, Last is True, Modified is True if it was written, and the page
   --  is no longer in the table: the caller writes it back to its region
   --  if it was modified, and frees it.

end Mem.Shared_Pages;
