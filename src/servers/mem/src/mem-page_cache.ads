with Rose.Addresses;
with Rose.Objects;

package Mem.Page_Cache is

   --  Physical pages that hold pages of read-only segments, by the store
   --  page they hold (#200). A fault on such a page that is already in
   --  memory maps the same physical page, without reading the store, so
   --  the processes that map it share it. Each cached page counts its
   --  mappings; one that nothing maps stays cached until its memory is
   --  needed.

   function Find
     (Page : Rose.Objects.Page_Object_Id)
      return Rose.Addresses.Physical_Page_Address;
   --  The physical page that holds the store page Page, or zero if none
   --  does.

   procedure Add
     (Page     : Rose.Objects.Page_Object_Id;
      Physical : Rose.Addresses.Physical_Page_Address);
   --  Cache Physical, which holds the store page Page and has just been
   --  mapped once. If the cache is full, Physical is not cached.

   procedure Acquire (Physical : Rose.Addresses.Physical_Page_Address);
   --  Count one more mapping of the cached page Physical.

   procedure Release
     (Physical : Rose.Addresses.Physical_Page_Address;
      Cached   : out Boolean);
   --  Count one mapping fewer of Physical. Cached is False if Physical is
   --  not a cached page, in which case the caller frees it.

   procedure Evict
     (Physical : out Rose.Addresses.Physical_Page_Address;
      Success  : out Boolean);
   --  Take out of the cache a page that nothing maps, so that its memory
   --  can be used again. Success is False if there is none.

end Mem.Page_Cache;
