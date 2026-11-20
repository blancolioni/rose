with System.Storage_Elements;

with Rose.Checkpoints;
with Rose.Console_IO;
with Rose.Limits;

with Mem.Calls;
with Mem.Change_Table;
with Mem.Page_Log;
with Mem.Page_Table;
with Mem.Physical_Map;
with Mem.Shared_Pages;

package body Mem.Log_Index is

   use Rose.Words;
   use type Rose.Addresses.Physical_Page_Address;
   use type Rose.Addresses.Virtual_Page_Address;
   use type Rose.Checkpoints.Header_Page_Flags;

   subtype Real_Process_Id is
     Process_Id range 1 .. Rose.Limits.Max_Processes;

   type Directory_Array is array (Real_Process_Id) of Word_32;

   Directories     : Directory_Array := (others => 0);
   --  The log page of each process's index directory in the last
   --  committed checkpoint, or 0.

   New_Directories : Directory_Array := (others => 0);
   --  The same, for the checkpoint being written.

   Entries_Per_Page : constant := Rose.Checkpoints.Index_Entries_Per_Page;

   --  A leaf is named by a key: the process's id times the number of
   --  leaves in a directory, plus the leaf's slot in the directory. So
   --  the keys of a process's leaves are together, in the order of their
   --  slots.

   No_Key       : constant Integer := Integer'Last;
   Before_First : constant Integer := -1;

   Max_Rewrites  : constant := 64;
   Rewrites      : array (1 .. Max_Rewrites) of Integer := (others => 0);
   Rewrite_Count : Natural := 0;
   --  The keys of the leaves that the next checkpoint writes again
   --  although none of their pages changed, with their directories,
   --  because the entries that hold their copies are to be freed
   --  (Rewrite, #952).

   subtype Page_Buffer is
     System.Storage_Elements.Storage_Array (1 .. Rose.Limits.Page_Size);

   Leaf_Buffer      : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;
   Directory_Buffer : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;
   Header_Buffer    : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;

   Window_Buffer    : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;
   --  Where a cached page is mapped while it is read.
   Spare_Buffer     : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;
   --  Where a page is read if the cache can't get memory for it.

   Leaf      : Rose.Checkpoints.Index_Page
     with Import, Address => Leaf_Buffer'Address;
   Directory : Rose.Checkpoints.Index_Page
     with Import, Address => Directory_Buffer'Address;
   Header    : Rose.Checkpoints.Index_Header_Page
     with Import, Address => Header_Buffer'Address;
   Window    : Rose.Checkpoints.Index_Page
     with Import, Address => Window_Buffer'Address;
   Spare     : Rose.Checkpoints.Index_Page
     with Import, Address => Spare_Buffer'Address;

   --  The cache of index pages (ADR 0028, section 3), by log page. An
   --  index page is never changed in place, so a cached copy is good for
   --  as long as the index names its log page. The pages are taken from
   --  the pages that mem manages, and given back under pressure (Evict).
   --  A page that Write_Index wrote is pending until the checkpoint ends:
   --  lookups don't find it, and it is dropped if the entry fails.

   Cache_Slots : constant := 32;

   type Slot_Count is range 0 .. Cache_Slots;
   subtype Slot_Index is Slot_Count range 1 .. Slot_Count'Last;

   type Cache_Slot is
      record
         Log_Page : Word_32 := 0;
         --  0 if the slot holds no page.
         Physical : Rose.Addresses.Physical_Page_Address := 0;
         --  0 if the slot has no memory.
         Last_Use : Natural := 0;
         Pending  : Boolean := False;
      end record;

   Cache : array (Slot_Index) of Cache_Slot;
   Clock : Natural := 0;

   procedure Add_Pending
     (Log_Page : Word_32;
      Buffer   : System.Address);
   --  Cache a copy of the page at Buffer, which Write_Index has just
   --  appended at Log_Page, as pending. Nothing happens if the cache
   --  can't get memory for it.

   procedure Copy_Page
     (From : Page_Buffer;
      To   : out Page_Buffer);
   --  Copy From to To one element at a time. An assignment between page
   --  overlays copies through a temporary on the stack, and mem's stack
   --  is one page.

   procedure Fill_Leaf
     (Key         : Integer;
      Batch_First : Word_32;
      Batch_Wrap  : Word_32);
   --  Set the entries of Leaf for the pages of the batch in leaf Key.

   function Find_Slot (Log_Page : Word_32) return Slot_Count;
   --  The slot that holds Log_Page and isn't pending, or 0.

   function Get_Entry
     (Log_Page : Word_32;
      Index    : Natural)
      return Word_32;
   --  Entry Index of the index page at Log_Page, through the cache.

   function Key_Of
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Integer;

   function New_Slot return Slot_Count;
   --  A slot with memory and no page, for a new page: one that is free,
   --  or one that gets memory now, or else the one used least recently
   --  that isn't pending. 0 if no slot has memory.

   function Next_Key (After : Integer) return Integer;
   --  The smallest key after After of a leaf with a page in the batch, or
   --  that is to be written again (Rewrites), or No_Key if there is
   --  none.

   function Process_Of (Key : Integer) return Process_Id;

   procedure Read_Page
     (Log_Page : Word_32;
      Buffer   : System.Address);
   --  Read log page Log_Page into the page buffer at Buffer, from the
   --  cache or the log, or fill it with zeroes if Log_Page is 0.

   function Slot_Of (Key : Integer) return Natural;

   procedure Touch (Slot : Slot_Index);
   --  Slot has just been used.

   -----------------
   -- Add_Pending --
   -----------------

   procedure Add_Pending
     (Log_Page : Word_32;
      Buffer   : System.Address)
   is
      Source : Page_Buffer
        with Import, Address => Buffer;
      Slot   : Slot_Count;
   begin
      --  A log page is used again only once nothing names it, so a copy
      --  of the page that was there before is of no use.

      for Item of Cache loop
         if Item.Log_Page = Log_Page then
            Item.Log_Page := 0;
            Item.Pending := False;
         end if;
      end loop;

      Slot := New_Slot;
      if Slot = 0 then
         return;
      end if;

      Mem.Calls.Load_Page (Cache (Slot).Physical, Window_Buffer'Address);
      Copy_Page (From => Source, To => Window_Buffer);
      Mem.Calls.Unload_Page (Cache (Slot).Physical, Window_Buffer'Address);
      Cache (Slot).Log_Page := Log_Page;
      Cache (Slot).Pending := True;
      Touch (Slot);
   end Add_Pending;

   ---------------
   -- Copy_Page --
   ---------------

   procedure Copy_Page
     (From : Page_Buffer;
      To   : out Page_Buffer)
   is
   begin
      for I in To'Range loop
         To (I) := From (I);
      end loop;
   end Copy_Page;

   --------------------
   -- End_Checkpoint --
   --------------------

   procedure End_Checkpoint (Committed : Boolean) is
   begin
      if Committed then
         Directories := New_Directories;
         Rewrite_Count := 0;

         --  The index names every page that the table of changes did
         --  (ADR 0028, section 4).

         Mem.Change_Table.Clear;
      else
         New_Directories := Directories;
      end if;

      for Item of Cache loop
         if Item.Pending then
            Item.Pending := False;
            if not Committed then
               Item.Log_Page := 0;
            end if;
         end if;
      end loop;
   end End_Checkpoint;

   -----------
   -- Evict --
   -----------

   procedure Evict
     (Physical : out Rose.Addresses.Physical_Page_Address;
      Success  : out Boolean)
   is
      Best : Slot_Count := 0;
   begin
      for I in Cache'Range loop
         if Cache (I).Physical /= 0
           and then
             (Best = 0
              or else Cache (I).Log_Page = 0
              or else (Cache (Best).Log_Page /= 0
                       and then Cache (I).Last_Use < Cache (Best).Last_Use))
         then
            Best := I;
         end if;
      end loop;

      if Best = 0 then
         Physical := 0;
         Success := False;
      else
         Physical := Cache (Best).Physical;
         Cache (Best) := (others => <>);
         Success := True;
      end if;
   end Evict;

   ---------------
   -- Fill_Leaf --
   ---------------

   procedure Fill_Leaf
     (Key         : Integer;
      Batch_First : Word_32;
      Batch_Wrap  : Word_32)
   is
      Index : Word_32 := 0;

      procedure Check (Position : Mem.Page_Table.Cursor);

      procedure Check_Change
        (Owner    : Process_Id;
         Page     : Rose.Addresses.Virtual_Page_Address;
         Log_Page : Word_32);

      -----------
      -- Check --
      -----------

      procedure Check (Position : Mem.Page_Table.Cursor) is
         Page : constant Rose.Addresses.Virtual_Page_Address :=
                  Mem.Page_Table.Virtual_Address (Position);
      begin
         if not Mem.Shared_Pages.Is_Shared
           (Mem.Page_Table.Physical_Address (Position))
         then
            if Key_Of (Mem.Page_Table.Process (Position), Page) = Key then
               --  The pages after the log wraps around are from log
               --  page 1 on.
               Leaf.Entries (Natural (Page mod Entries_Per_Page)) :=
                 (if Batch_Wrap /= 0 and then Index >= Batch_Wrap
                  then 1 + (Index - Batch_Wrap)
                  else Batch_First + Index);
            end if;
            Index := Index + 1;
         end if;
      end Check;

      ------------------
      -- Check_Change --
      ------------------

      procedure Check_Change
        (Owner    : Process_Id;
         Page     : Rose.Addresses.Virtual_Page_Address;
         Log_Page : Word_32)
      is
      begin
         if Key_Of (Owner, Page) = Key then
            Leaf.Entries (Natural (Page mod Entries_Per_Page)) := Log_Page;
         end if;
      end Check_Change;

   begin
      --  The pages paged out since the last checkpoint first, then the
      --  batch: a page that changed again after it was paged out takes
      --  the batch's copy.

      Mem.Change_Table.Iterate (Check_Change'Access);
      Mem.Page_Table.Iterate_Dirty_Pages (Check'Access);
   end Fill_Leaf;

   ----------
   -- Find --
   ----------

   function Find
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Rose.Words.Word_32
   is
      Leaf_Page : Word_32;
      Changed   : constant Word_32 := Mem.Change_Table.Find (Process, Page);
   begin
      --  A page paged out since the last checkpoint is in the table of
      --  changes, and not yet in the index (ADR 0028, section 3).

      if Changed /= 0 then
         return Changed;
      end if;

      --  Is_Available also opens the Page_Log client, which nothing
      --  else has done yet on a boot that restores a checkpoint.

      if Process not in Real_Process_Id
        or else Directories (Process) = 0
        or else not Mem.Page_Log.Is_Available
      then
         return 0;
      end if;

      Leaf_Page :=
        Get_Entry (Directories (Process),
                   Natural (Page / Entries_Per_Page));

      if Leaf_Page = 0 then
         return 0;
      end if;

      return Get_Entry (Leaf_Page, Natural (Page mod Entries_Per_Page));
   end Find;

   ---------------
   -- Find_Slot --
   ---------------

   function Find_Slot (Log_Page : Word_32) return Slot_Count is
   begin
      for I in Cache'Range loop
         if Cache (I).Log_Page = Log_Page
           and then not Cache (I).Pending
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Slot;

   ------------
   -- Forget --
   ------------

   procedure Forget (Process : Process_Id) is
   begin
      if Process in Real_Process_Id then
         Directories (Process) := 0;
         New_Directories (Process) := 0;
      end if;
      Mem.Change_Table.Forget (Process);

      declare
         Kept : Natural := 0;
      begin
         for I in 1 .. Rewrite_Count loop
            if Process_Of (Rewrites (I)) /= Process then
               Kept := Kept + 1;
               Rewrites (Kept) := Rewrites (I);
            end if;
         end loop;
         Rewrite_Count := Kept;
      end;
   end Forget;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
     (Log_Page : Word_32;
      Index    : Natural)
      return Word_32
   is
      Slot   : Slot_Count := Find_Slot (Log_Page);
      Result : Word_32;
   begin
      if Slot = 0 then
         Slot := New_Slot;
         if Slot = 0 then
            Mem.Page_Log.Read (Log_Page, Spare_Buffer'Address);
            return Spare.Entries (Index);
         end if;
         Mem.Calls.Load_Page (Cache (Slot).Physical, Window_Buffer'Address);
         Mem.Page_Log.Read (Log_Page, Window_Buffer'Address);
         Cache (Slot).Log_Page := Log_Page;
      else
         Mem.Calls.Load_Page (Cache (Slot).Physical, Window_Buffer'Address);
      end if;

      Result := Window.Entries (Index);
      Mem.Calls.Unload_Page (Cache (Slot).Physical, Window_Buffer'Address);
      Touch (Slot);
      return Result;
   end Get_Entry;

   ------------------------
   -- Is_Live_Index_Page --
   ------------------------

   function Is_Live_Index_Page
     (Process  : Process_Id;
      Slot     : Rose.Words.Word_16;
      Log_Page : Rose.Words.Word_32)
      return Boolean
   is
   begin
      if Process not in Real_Process_Id
        or else Directories (Process) = 0
        or else Log_Page = 0
      then
         return False;
      elsif Slot = Rose.Checkpoints.Directory_Slot then
         return Directories (Process) = Log_Page;
      elsif Natural (Slot) >= Entries_Per_Page then
         return False;
      else
         return Get_Entry (Directories (Process), Natural (Slot))
           = Log_Page;
      end if;
   end Is_Live_Index_Page;

   ------------
   -- Key_Of --
   ------------

   function Key_Of
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Integer
   is
   begin
      return Integer (Process) * Entries_Per_Page
        + Integer (Page / Entries_Per_Page);
   end Key_Of;

   --------------
   -- New_Slot --
   --------------

   function New_Slot return Slot_Count is
      Best : Slot_Count := 0;
   begin
      for I in Cache'Range loop
         if Cache (I).Physical /= 0 and then Cache (I).Log_Page = 0 then
            return I;
         end if;
      end loop;

      for I in Cache'Range loop
         if Cache (I).Physical = 0 then
            declare
               Page    : Rose.Addresses.Physical_Page_Address;
               Success : Boolean;
            begin
               Mem.Physical_Map.Allocate_Page (Page, Success);
               if Success then
                  Cache (I).Physical := Page;
                  return I;
               end if;
            end;
            exit;
         end if;
      end loop;

      for I in Cache'Range loop
         if Cache (I).Physical /= 0
           and then not Cache (I).Pending
           and then
             (Best = 0 or else Cache (I).Last_Use < Cache (Best).Last_Use)
         then
            Best := I;
         end if;
      end loop;

      if Best /= 0 then
         Cache (Best).Log_Page := 0;
      end if;

      return Best;
   end New_Slot;

   --------------
   -- Next_Key --
   --------------

   function Next_Key (After : Integer) return Integer is
      Result : Integer := No_Key;

      procedure Check (Position : Mem.Page_Table.Cursor);

      procedure Check_Change
        (Owner    : Process_Id;
         Page     : Rose.Addresses.Virtual_Page_Address;
         Log_Page : Word_32);

      -----------
      -- Check --
      -----------

      procedure Check (Position : Mem.Page_Table.Cursor) is
         Key : Integer;
      begin
         if not Mem.Shared_Pages.Is_Shared
           (Mem.Page_Table.Physical_Address (Position))
         then
            Key := Key_Of (Mem.Page_Table.Process (Position),
                           Mem.Page_Table.Virtual_Address (Position));
            if Key > After and then Key < Result then
               Result := Key;
            end if;
         end if;
      end Check;

      ------------------
      -- Check_Change --
      ------------------

      procedure Check_Change
        (Owner    : Process_Id;
         Page     : Rose.Addresses.Virtual_Page_Address;
         Log_Page : Word_32)
      is
         pragma Unreferenced (Log_Page);
         Key : constant Integer := Key_Of (Owner, Page);
      begin
         if Key > After and then Key < Result then
            Result := Key;
         end if;
      end Check_Change;

   begin
      Mem.Page_Table.Iterate_Dirty_Pages (Check'Access);
      Mem.Change_Table.Iterate (Check_Change'Access);
      for Key of Rewrites (1 .. Rewrite_Count) loop
         if Key > After and then Key < Result then
            Result := Key;
         end if;
      end loop;
      return Result;
   end Next_Key;

   ----------------
   -- Process_Of --
   ----------------

   function Process_Of (Key : Integer) return Process_Id is
   begin
      return Process_Id (Key / Entries_Per_Page);
   end Process_Of;

   ---------------
   -- Read_Page --
   ---------------

   procedure Read_Page
     (Log_Page : Word_32;
      Buffer   : System.Address)
   is
      Page : Page_Buffer
        with Import, Address => Buffer;
      Slot : constant Slot_Count :=
               (if Log_Page = 0 then 0 else Find_Slot (Log_Page));
   begin
      if Log_Page = 0 then
         Page := (others => 0);
      elsif Slot /= 0 then
         Mem.Calls.Load_Page (Cache (Slot).Physical, Window_Buffer'Address);
         Copy_Page (From => Window_Buffer, To => Page);
         Mem.Calls.Unload_Page (Cache (Slot).Physical, Window_Buffer'Address);
         Touch (Slot);
      else
         Mem.Page_Log.Read (Log_Page, Buffer);
      end if;
   end Read_Page;

   -------------
   -- Rewrite --
   -------------

   procedure Rewrite
     (Process : Process_Id;
      Slot    : Rose.Words.Word_16;
      Success : out Boolean)
   is
      Leaf_Slot : Natural := Natural (Slot);
      Key       : Integer;
   begin
      Success := True;

      if Process not in Real_Process_Id
        or else Directories (Process) = 0
      then
         return;
      end if;

      --  A directory is written after the leaves of its process that
      --  are written, so writing one of its leaves again writes it
      --  again too. It names at least one, or it wouldn't have been
      --  written. Nothing else uses Directory between checkpoints.

      if Slot = Rose.Checkpoints.Directory_Slot then
         Read_Page (Directories (Process), Directory_Buffer'Address);
         Leaf_Slot := Entries_Per_Page;
         for I in Directory.Entries'Range loop
            if Directory.Entries (I) /= 0 then
               Leaf_Slot := I;
               exit;
            end if;
         end loop;
      end if;

      if Leaf_Slot >= Entries_Per_Page then
         return;
      end if;

      Key := Integer (Process) * Entries_Per_Page + Leaf_Slot;

      for Item of Rewrites (1 .. Rewrite_Count) loop
         if Item = Key then
            return;
         end if;
      end loop;

      if Rewrite_Count = Max_Rewrites then
         Success := False;
         return;
      end if;

      Rewrite_Count := Rewrite_Count + 1;
      Rewrites (Rewrite_Count) := Key;
   end Rewrite;

   ---------------------
   -- Saved_Directory --
   ---------------------

   function Saved_Directory
     (Process : Process_Id)
      return Rose.Words.Word_32
   is
   begin
      if Process in Real_Process_Id then
         return New_Directories (Process);
      else
         return 0;
      end if;
   end Saved_Directory;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory
     (Process   : Process_Id;
      Directory : Rose.Words.Word_32)
   is
   begin
      if Process in Real_Process_Id then
         Directories (Process) := Directory;
         New_Directories (Process) := Directory;
      end if;
   end Set_Directory;

   -------------
   -- Slot_Of --
   -------------

   function Slot_Of (Key : Integer) return Natural is
   begin
      return Key mod Entries_Per_Page;
   end Slot_Of;

   -----------
   -- Touch --
   -----------

   procedure Touch (Slot : Slot_Index) is
   begin
      Clock := Clock + 1;
      Cache (Slot).Last_Use := Clock;
   end Touch;

   -----------------
   -- Write_Index --
   -----------------

   procedure Write_Index
     (Batch_First : Rose.Words.Word_32;
      Batch_Wrap  : Rose.Words.Word_32;
      Success     : out Boolean)
   is
      Log_Page : Word_32 := 0;
      --  Where the last page appended went.

      procedure Add_Reference
        (Process : Process_Id;
         Slot    : Word_16);

      procedure Append (Buffer : System.Address);

      procedure Reset_Header;

      -------------------
      -- Add_Reference --
      -------------------

      procedure Add_Reference
        (Process : Process_Id;
         Slot    : Word_16)
      is
      begin
         if Header.Count = Rose.Checkpoints.Max_Index_References then
            Append (Header_Buffer'Address);
            Reset_Header;
         end if;

         Header.Count := Header.Count + 1;
         Header.Pages (Header.Count) :=
           (Process => Word_16 (Process), Slot => Slot);
      end Add_Reference;

      ------------
      -- Append --
      ------------

      procedure Append (Buffer : System.Address) is
      begin
         if Success then
            Log_Page := Mem.Page_Log.Append (Buffer);
            if Log_Page = 0 then
               Success := False;
            end if;
         end if;
      end Append;

      ------------------
      -- Reset_Header --
      ------------------

      procedure Reset_Header is
      begin
         Header_Buffer := (others => 0);
         Header.Magic := Rose.Checkpoints.Index_Header_Magic;
      end Reset_Header;

      Key     : Integer := Next_Key (Before_First);
      Next    : Integer;
      Process : Process_Id := 0;
      --  The process whose directory is in Directory.

   begin
      New_Directories := Directories;
      Success := True;

      if Key = No_Key then
         return;
      end if;

      --  The header pages list every page of the section, in the order
      --  they are written: each process's leaves, then its directory.

      Reset_Header;

      while Key /= No_Key loop
         Next := Next_Key (Key);
         Add_Reference (Process_Of (Key), Word_16 (Slot_Of (Key)));
         if Next = No_Key or else Process_Of (Next) /= Process_Of (Key) then
            Add_Reference (Process_Of (Key), Rose.Checkpoints.Directory_Slot);
         end if;
         Key := Next;
      end loop;

      Header.Flags := Header.Flags or Rose.Checkpoints.Is_Last_Header_Page;
      Append (Header_Buffer'Address);

      --  A leaf starts as a copy of the one it replaces, so it keeps the
      --  pages of its 4 MiB that the batch doesn't have. The directory
      --  is copied in the same way.

      Key := Next_Key (Before_First);
      while Success and then Key /= No_Key loop
         if Process_Of (Key) not in Real_Process_Id then
            Rose.Console_IO.Put_Line ("mem: index: bad process id");
            Success := False;
            return;
         end if;

         if Process_Of (Key) /= Process then
            Process := Process_Of (Key);
            Read_Page (Directories (Process), Directory_Buffer'Address);
         end if;

         Read_Page (Directory.Entries (Slot_Of (Key)), Leaf_Buffer'Address);
         Fill_Leaf (Key, Batch_First, Batch_Wrap);
         Append (Leaf_Buffer'Address);
         Directory.Entries (Slot_Of (Key)) := Log_Page;
         if Success then
            Add_Pending (Log_Page, Leaf_Buffer'Address);
         end if;

         Next := Next_Key (Key);
         if Next = No_Key or else Process_Of (Next) /= Process then
            Append (Directory_Buffer'Address);
            New_Directories (Process) := Log_Page;
            if Success then
               Add_Pending (Log_Page, Directory_Buffer'Address);
            end if;
         end if;
         Key := Next;
      end loop;
   end Write_Index;

end Mem.Log_Index;
