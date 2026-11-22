with System.Storage_Elements;

with Rose.Checkpoints;
with Rose.Limits;

with Mem.Calls;
with Mem.Change_Table;
with Mem.Log_Index;
with Mem.Page_Cache;
with Mem.Page_Log;
with Mem.Page_Table;
with Mem.Physical_Map;
with Mem.Processes;
with Mem.Shared_Pages;

package body Mem.Virtual_Map is

   use type System.Storage_Elements.Storage_Offset;

   Page_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;

   Batch_Pages : constant := Rose.Limits.Page_Out_Batch_Pages;

   Batch_Window : System.Storage_Elements.Storage_Array
     (1 .. (Batch_Pages + 1) * Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   --  A batch of changed pages is a header page, here, and then the pages
   --  it lists, mapped one after another, so that the log gets them in
   --  one call (#974).

   Batch_Header : Rose.Checkpoints.Header_Page
     with Import, Address => Batch_Window'Address;

   function Batch_Page (Index : Natural) return System.Address
   is (Batch_Window (Batch_Window'First
                     + System.Storage_Elements.Storage_Offset (Index)
                     * Rose.Limits.Page_Size)
       'Address);
   --  Where page Index of a batch goes, from 0 for the header.

   Stopped : Boolean := False;
   --  The log refused a batch, because a checkpoint was being written or
   --  an append failed. No more are written until a checkpoint ends.

   function Has_Room return Boolean
   is (not Stopped
       and then Mem.Change_Table.Room >= Batch_Pages
       and then Mem.Page_Log.Is_Available);
   --  A batch of changed pages can be written.

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

   function Is_Candidate (Position : Mem.Page_Table.Cursor) return Boolean;
   --  The page at Position is of a persistent, writable segment, and is
   --  not shared: one that mem can page out, if it is clean or changed.

   procedure Write_Changed_Pages (Success : out Boolean);
   --  Page out a batch of changed pages: unmap them, append them to the
   --  open entry in one call, add them to the table of changes, and free
   --  them (ADR 0028, section 5; #974). If the log refuses the batch, the
   --  pages are mapped again, still changed, and Stopped is set. Success
   --  is False if no page was freed.

   procedure Keep_Entries;
   --  If Mem.Page_Table can track fewer than Rose.Limits.Reserve_Low_Pages
   --  more pages, free clean pages, and then changed pages, until it can
   --  track Rose.Limits.Reserve_High_Pages, or there are no more (#591).
   --  The table holds Rose.Limits.Max_Mapped_Pages, which is fewer than
   --  a machine of more than 32 MB has, so it can fill while there are
   --  free pages. Cached pages aren't evicted: they aren't in the table.

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

   --------------
   -- Can_Copy --
   --------------

   function Can_Copy return Boolean is
   begin
      return Has_Room
        and then Mem.Change_Table.Count < Mem.Change_Table.Capacity / 2;
   end Can_Copy;

   ---------------------
   -- Changes_Stopped --
   ---------------------

   function Changes_Stopped return Boolean is
   begin
      return not Has_Room;
   end Changes_Stopped;

   ------------------
   -- Copy_Forward --
   ------------------

   procedure Copy_Forward
     (Pages   : Copied_Pages;
      Success : out Boolean)
   is
      use type Rose.Words.Word_32;
      Count : constant Natural := Pages'Length;
      First : Rose.Words.Word_32;
      Wrap  : Rose.Words.Word_32;
   begin
      Success := False;

      if Count = 0 or else Count > Batch_Pages or else not Has_Room then
         return;
      end if;

      --  Field by field: a whole-page aggregate would be built on mem's
      --  one-page stack.

      Batch_Header.Magic := Rose.Checkpoints.Header_Page_Magic;
      for Element of Batch_Header.Pages loop
         Element := (others => <>);
      end loop;
      Batch_Header.Flags := Rose.Checkpoints.Is_Last_Header_Page;
      Batch_Header.Count := Rose.Words.Word_32 (Count);

      for I in 1 .. Count loop
         declare
            Item : Copied_Page renames Pages (Pages'First + I - 1);
         begin
            Batch_Header.Pages (Rose.Words.Word_32 (I)) :=
              (Process => Rose.Words.Word_16 (Item.Process),
               Page    => Item.Page);
            Mem.Page_Log.Read (Item.Log_Page, Batch_Page (I));
         end;
      end loop;

      Mem.Page_Log.Append_Batch (Batch_Page (0), Count + 1, First, Wrap);

      if First = 0 then
         Stopped := True;
         return;
      end if;

      --  Page I of the batch is at index I, after the header at 0.

      for I in 1 .. Count loop
         declare
            Item  : Copied_Page renames Pages (Pages'First + I - 1);
            Index : constant Rose.Words.Word_32 := Rose.Words.Word_32 (I);
         begin
            Mem.Change_Table.Add
              (Process  => Item.Process,
               Page     => Item.Page,
               Log_Page =>
                 (if Wrap /= 0 and then Index >= Wrap
                  then 1 + (Index - Wrap)
                  else First + Index));
         end;
      end loop;

      Success := True;
   end Copy_Forward;

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
      begin
         Taken := False;

         if not Is_Candidate (Position) then
            return;
         end if;

         --  The kernel refuses a page of a buffer that the process has
         --  lent to a server in its call in progress, and says so in its
         --  log (#972). The page is passed over.

         Mem.Calls.Unmap
           (Process  =>
              Mem.Processes.Process_Object_Id
                (Mem.Page_Table.Process (Position)),
            Virtual  => Mem.Page_Table.Virtual_Address (Position),
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

   --------------------
   -- End_Checkpoint --
   --------------------

   procedure End_Checkpoint is
   begin
      Stopped := False;
   end End_Checkpoint;

   ------------------
   -- Is_Candidate --
   ------------------

   function Is_Candidate (Position : Mem.Page_Table.Cursor) return Boolean
   is
      use type Rose.Objects.Object_Id;
      Pid  : constant Process_Id := Mem.Page_Table.Process (Position);
      Virt : constant Rose.Addresses.Virtual_Page_Address :=
               Mem.Page_Table.Virtual_Address (Position);
      Page_Object : Rose.Objects.Object_Id;
      Valid, Readable, Writable, Executable, Persistent : Boolean;
   begin
      if Mem.Shared_Pages.Is_Shared
        (Mem.Page_Table.Physical_Address (Position))
      then
         return False;
      end if;

      Mem.Processes.Get_Process_Segment
        (Pid, Virt, Page_Object,
         Valid, Readable, Writable, Executable, Persistent);

      return Valid
        and then Writable
        and then Persistent
        and then Mem.Processes.Shared_Page (Pid, Virt) = 0;
   end Is_Candidate;

   ------------------
   -- Keep_Entries --
   ------------------

   procedure Keep_Entries is
      Page  : Rose.Addresses.Physical_Page_Address;
      Freed : Boolean;
   begin
      --  Nothing here writes to the console, as for Keep_Reserve.

      if Mem.Page_Table.Free_Entries >= Rose.Limits.Reserve_Low_Pages then
         return;
      end if;

      while Mem.Page_Table.Free_Entries < Rose.Limits.Reserve_High_Pages
      loop
         Drop_Clean_Page (Page, Freed);
         if Freed then
            Mem.Physical_Map.Deallocate_Page (Page);
         else
            Write_Changed_Pages (Freed);
            exit when not Freed;
         end if;
      end loop;
   end Keep_Entries;

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
         if Freed then
            Mem.Physical_Map.Deallocate_Page (Page);
         else
            Write_Changed_Pages (Freed);
            exit when not Freed;
         end if;
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
      --  Room in the table first, so that the page is tracked. The page
      --  being mapped isn't in the table yet, so it isn't freed (#591).

      Keep_Entries;

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
      if not Success then
         Write_Changed_Pages (Success);
         if Success then
            Mem.Physical_Map.Allocate_Page (Physical_Page, Success);
         end if;
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

   -------------------------
   -- Write_Changed_Pages --
   -------------------------

   procedure Write_Changed_Pages (Success : out Boolean) is
      use type Rose.Words.Word_32;

      Batch : array (1 .. Batch_Pages) of Mem.Page_Table.Cursor;
      Count : Natural := 0;

      procedure Try_Page
        (Position : Mem.Page_Table.Cursor;
         Taken    : out Boolean);

      function Window (Index : Natural) return System.Address
      is (Batch_Window (Batch_Window'First
                        + System.Storage_Elements.Storage_Offset (Index)
                        * Rose.Limits.Page_Size)
          'Address);
      --  Where page Index of the batch goes, from 0 for the header.

      --------------
      -- Try_Page --
      --------------

      procedure Try_Page
        (Position : Mem.Page_Table.Cursor;
         Taken    : out Boolean)
      is
         use type Mem.Page_Table.Cursor;
      begin
         Taken := False;

         if not Is_Candidate (Position) then
            return;
         end if;

         for I in 1 .. Count loop
            if Batch (I) = Position then
               return;
            end if;
         end loop;

         --  Unmapped first, so that the process can't change the page
         --  while it is written. A page lent to a call is refused, and
         --  passed over (#972).

         Mem.Calls.Unmap
           (Process  =>
              Mem.Processes.Process_Object_Id
                (Mem.Page_Table.Process (Position)),
            Virtual  => Mem.Page_Table.Virtual_Address (Position),
            Unmapped => Taken);
      end Try_Page;

      Position : Mem.Page_Table.Cursor;
      First    : Rose.Words.Word_32;
      Wrap     : Rose.Words.Word_32;

   begin
      Success := False;

      --  Nothing here writes to the console, as for Keep_Reserve.

      if not Has_Room then
         return;
      end if;

      while Count < Batch_Pages loop
         Mem.Page_Table.Sweep_Changed_Pages (Try_Page'Access, Position);
         exit when not Mem.Page_Table.Has_Element (Position);
         Count := Count + 1;
         Batch (Count) := Position;
      end loop;

      if Count = 0 then
         return;
      end if;

      --  Field by field: a whole-page aggregate would be built on mem's
      --  one-page stack.

      Batch_Header.Magic := Rose.Checkpoints.Header_Page_Magic;
      for Element of Batch_Header.Pages loop
         Element := (others => <>);
      end loop;
      Batch_Header.Flags := Rose.Checkpoints.Is_Last_Header_Page;
      Batch_Header.Count := Rose.Words.Word_32 (Count);
      for I in 1 .. Count loop
         Batch_Header.Pages (Rose.Words.Word_32 (I)) :=
           (Process =>
              Rose.Words.Word_16 (Mem.Page_Table.Process (Batch (I))),
            Page    => Mem.Page_Table.Virtual_Address (Batch (I)));
         Mem.Calls.Load_Page
           (Mem.Page_Table.Physical_Address (Batch (I)), Window (I));
      end loop;

      Mem.Page_Log.Append_Batch (Window (0), Count + 1, First, Wrap);

      for I in 1 .. Count loop
         Mem.Calls.Unload_Page
           (Mem.Page_Table.Physical_Address (Batch (I)), Window (I));
      end loop;

      if First = 0 then

         --  The pages are still changed, and in the dirty page list, so
         --  they are mapped again read only: a write makes them writable
         --  again, as after a checkpoint.

         Stopped := True;
         for Item of Batch (1 .. Count) loop
            Mem.Calls.Map
              (Process    =>
                 Mem.Processes.Process_Object_Id
                   (Mem.Page_Table.Process (Item)),
               Physical   => Mem.Page_Table.Physical_Address (Item),
               Virtual    => Mem.Page_Table.Virtual_Address (Item),
               Readable   => Mem.Page_Table.Is_Readable (Item),
               Writeable  => False,
               Executable => Mem.Page_Table.Is_Executable (Item));
            Mem.Page_Table.Set_Read_Only (Item);
         end loop;
         return;
      end if;

      --  Page I of the batch is at index I, after the header at 0.

      for I in 1 .. Count loop
         declare
            Item     : constant Mem.Page_Table.Cursor := Batch (I);
            Index    : constant Rose.Words.Word_32 := Rose.Words.Word_32 (I);
            Physical : constant Rose.Addresses.Physical_Page_Address :=
                         Mem.Page_Table.Physical_Address (Item);
         begin
            Mem.Change_Table.Add
              (Process  => Mem.Page_Table.Process (Item),
               Page     => Mem.Page_Table.Virtual_Address (Item),
               Log_Page =>
                 (if Wrap /= 0 and then Index >= Wrap
                  then 1 + (Index - Wrap)
                  else First + Index));
            Mem.Page_Table.Delete (Item);
            Mem.Physical_Map.Deallocate_Page (Physical);
         end;
      end loop;

      Success := True;
   end Write_Changed_Pages;

end Mem.Virtual_Map;
