with System;

with Rose.Addresses;
with Rose.Objects;

package Mem.Virtual_Map is

   procedure Allocate_Page
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean);
   --  A physical page for a process or a restore: a free one, or else one
   --  that Reclaim frees. Then, if fewer than Rose.Limits.Reserve_Low_Pages
   --  are free, free pages until Rose.Limits.Reserve_High_Pages are
   --  (#973). Success is False if there is no page.

   procedure Reclaim
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean);
   --  Free a page that mem can read again when it is needed: a cached
   --  page that nothing maps, a page of the cache of index pages, a clean
   --  page of a process (#973), or else a batch of changed pages, paged
   --  out to the log (#974).

   function Changes_Stopped return Boolean;
   --  No more changed pages can be paged out until a checkpoint ends:
   --  the table of changes is full, or the log refused a batch (ADR 0028,
   --  section 6; #974). Only clean pages can be freed.

   procedure End_Checkpoint;
   --  mem's part of a checkpoint is done, so changed pages can be paged
   --  out again.

   procedure Map (Process       : Rose.Objects.Object_Id;
                  Physical_Page : Rose.Addresses.Physical_Page_Address;
                  Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
                  Readable      : Boolean;
                  Writable      : Boolean;
                  Executable    : Boolean;
                  Persistent    : Boolean);

   procedure Remove_All
     (Process : Rose.Objects.Capability_Identifier);

   procedure With_Page
     (Page    : Rose.Addresses.Physical_Page_Address;
      Process : not null access
        procedure (Address : System.Address));

end Mem.Virtual_Map;
