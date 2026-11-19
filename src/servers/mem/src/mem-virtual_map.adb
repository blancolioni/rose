with System.Storage_Elements;

with Rose.Limits;
with Rose.Words;

with Mem.Calls;
with Mem.Log_Index;
with Mem.Page_Cache;
with Mem.Page_Log;
with Mem.Page_Table;
with Mem.Physical_Map;
with Mem.Processes;
with Mem.Shared_Pages;

package body Mem.Virtual_Map is

   Page_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;

   procedure Drop_Clean_Page
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean);
   --  Unmap a clean page of a process, forget it, and return its physical
   --  page, which the caller frees or uses (ADR 0028, section 5; #973). A
   --  clean page has a copy in the log or in its segment's region, which
   --  a later fault reads, so it can be dropped without writing anything.
   --  Only pages of persistent, writable segments that are not shared are
   --  dropped: other pages have no copy, or are shared, and boot modules
   --  are not in mem's page table. Success is False if there is none.

   procedure Keep_Reserve;
   --  If fewer than Reserve_Low_Pages are free, free cached pages that
   --  nothing maps, and then clean pages, until Reserve_High_Pages are
   --  free, or there are no more.

   -------------------
   -- Allocate_Page --
   -------------------

   procedure Allocate_Page
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean)
   is
   begin
      Mem.Physical_Map.Allocate_Page (Physical_Page, Success);
      if not Success then
         Reclaim (Physical_Page, Success);
      end if;

      if Success then
         Keep_Reserve;
      end if;
   end Allocate_Page;

   ---------------------
   -- Drop_Clean_Page --
   ---------------------

   procedure Drop_Clean_Page
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean)
   is
      procedure Try_Page
        (Position : Mem.Page_Table.Cursor;
         Taken    : out Boolean);

      --------------
      -- Try_Page --
      --------------

      procedure Try_Page
        (Position : Mem.Page_Table.Cursor;
         Taken    : out Boolean)
      is
         use type Rose.Objects.Object_Id;
         Pid  : constant Process_Id := Mem.Page_Table.Process (Position);
         Virt : constant Rose.Addresses.Virtual_Page_Address :=
                  Mem.Page_Table.Virtual_Address (Position);
         Page_Object : Rose.Objects.Object_Id;
         Valid, Readable, Writable, Executable, Persistent : Boolean;
      begin
         Taken := False;

         if Mem.Shared_Pages.Is_Shared
           (Mem.Page_Table.Physical_Address (Position))
         then
            return;
         end if;

         Mem.Processes.Get_Process_Segment
           (Pid, Virt, Page_Object,
            Valid, Readable, Writable, Executable, Persistent);

         if not Valid
           or else not Writable
           or else not Persistent
           or else Mem.Processes.Shared_Page (Pid, Virt) /= 0
         then
            return;
         end if;

         --  The kernel refuses a page of a buffer that the process has
         --  lent to a server in its call in progress, and says so in its
         --  log (#972). The page is passed over.

         Mem.Calls.Unmap
           (Process  => Mem.Processes.Process_Object_Id (Pid),
            Virtual  => Virt,
            Unmapped => Taken);
      end Try_Page;

      Position : Mem.Page_Table.Cursor;

   begin
      Physical_Page := 0;
      Success := False;

      --  A dropped page is read again from the log through its process's
      --  index, which needs the log.

      if not Mem.Page_Log.Is_Available then
         return;
      end if;

      Mem.Page_Table.Sweep_Clean_Pages (Try_Page'Access, Position);

      if Mem.Page_Table.Has_Element (Position) then
         Physical_Page := Mem.Page_Table.Physical_Address (Position);
         Mem.Page_Table.Delete (Position);
         Success := True;
      end if;
   end Drop_Clean_Page;

   ------------------
   -- Keep_Reserve --
   ------------------

   procedure Keep_Reserve is
      use type Rose.Words.Word;
      Page  : Rose.Addresses.Physical_Page_Address;
      Freed : Boolean;
   begin
      --  Nothing here writes to the console. The console calls a driver
      --  that is a launched process, whose pages mem may have dropped, so
      --  a call to the console can wait on a fault that only mem can
      --  handle (as with the store in #914).

      if Mem.Physical_Map.Available_Pages >= Rose.Limits.Reserve_Low_Pages
      then
         return;
      end if;

      while Mem.Physical_Map.Available_Pages
        < Rose.Limits.Reserve_High_Pages
      loop
         Mem.Page_Cache.Evict (Page, Freed);
         if not Freed then
            Drop_Clean_Page (Page, Freed);
         end if;
         exit when not Freed;
         Mem.Physical_Map.Deallocate_Page (Page);
      end loop;
   end Keep_Reserve;

   ---------
   -- Map --
   ---------

   procedure Map (Process       : Rose.Objects.Object_Id;
                  Physical_Page : Rose.Addresses.Physical_Page_Address;
                  Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
                  Readable      : Boolean;
                  Writable      : Boolean;
                  Executable    : Boolean;
                  Persistent    : Boolean)
   is
      pragma Unreferenced (Writable);
   begin
      Mem.Calls.Map
        (Process    => Process,
         Physical   => Physical_Page,
         Virtual    => Virtual_Page,
         Readable   => Readable,
         Writeable  => False,
         Executable => Executable);
      Mem.Page_Table.Insert
        (Process    => Mem.Processes.Get_Process_Id (Process),
         Physical   => Physical_Page,
         Virtual    => Virtual_Page,
         Mapped     => True,
         Readable   => True,
         Writable   => False,
         Executable => Executable,
         Persistent => Persistent);
   end Map;

   -------------
   -- Reclaim --
   -------------

   procedure Reclaim
     (Physical_Page : out Rose.Addresses.Physical_Page_Address;
      Success       : out Boolean)
   is
   begin
      --  Read-only pages first, then index pages: a lookup that misses
      --  the cache of index pages costs up to two reads of the log. Then
      --  a clean page of a process, which costs one (#973).
      Mem.Page_Cache.Evict (Physical_Page, Success);
      if not Success then
         Mem.Log_Index.Evict (Physical_Page, Success);
      end if;
      if not Success then
         Drop_Clean_Page (Physical_Page, Success);
      end if;
   end Reclaim;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All
     (Process : Rose.Objects.Capability_Identifier)
   is null;

   ---------------
   -- With_Page --
   ---------------

   procedure With_Page
     (Page    : Rose.Addresses.Physical_Page_Address;
      Process : not null access
        procedure (Address : System.Address))
   is
   begin
      Mem.Calls.Load_Page (Page, Page_Buffer'Address);
      Process (Page_Buffer'Address);
      Mem.Calls.Unload_Page (Page, Page_Buffer'Address);
   end With_Page;

end Mem.Virtual_Map;
