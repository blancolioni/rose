with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Limits;
with Rose.Trace;
with Rose.Words;

with Mem.Calls;
with Mem.Page_Cache;
with Mem.Physical_Map;
with Mem.Processes;
with Mem.Shared_Pages;

package body Mem.Page_Table is

   --  The table is sized when mem starts, from its physical pages, and
   --  mapped into mem at these addresses, from mem's own pages (#998).
   --  Each part has room for the largest table, and only the pages that
   --  this one needs are mapped: Max_Entries is twice the pages of a
   --  32-bit physical address space.

   Max_Entries : constant := 2 * 2 ** 20;
   Max_Bucket_Bits : constant := 20;

   Mapped_Base : constant := 16#1000_0000#;  --  Max_Entries * 8 bytes
   Next_Base   : constant := 16#1100_0000#;  --  Max_Entries * 4 bytes
   Dirty_Base  : constant := 16#1180_0000#;  --  Max_Entries * 4 bytes
   Bucket_Base : constant := 16#1200_0000#;  --  2 ** 20 * 4 bytes

   type Mapped_Page_Record is
      record
         Process    : Process_Id;
         Virt       : Rose.Addresses.Virtual_Page_Address;
         Phys       : Rose.Addresses.Physical_Page_Address;
         Dirty      : Boolean;
         Mapped     : Boolean;
         Readable   : Boolean;
         Writable   : Boolean;
         Executable : Boolean;
         Persistent : Boolean;
      end record
   with Pack, Size => 64;

   type Mapped_Page_Array is
     array (Cursor range 1 .. Max_Entries) of Mapped_Page_Record
     with Pack;

   type Cursor_Array is array (Cursor range 1 .. Max_Entries) of Cursor;

   type Dirty_Page_Array is array (1 .. Max_Entries) of Cursor;

   type Bucket_Array is
     array (Natural range 0 .. 2 ** Max_Bucket_Bits - 1) of Cursor;

   Mapped_Pages : Mapped_Page_Array
     with Import, Address => System'To_Address (Mapped_Base);

   Next_Page : Cursor_Array
     with Import, Address => System'To_Address (Next_Base);
   --  The next entry in the same bucket, or, for a free entry, the next
   --  free entry.

   Dirty_Pages : Dirty_Page_Array
     with Import, Address => System'To_Address (Dirty_Base);

   Buckets : Bucket_Array
     with Import, Address => System'To_Address (Bucket_Base);
   --  The first entry of each bucket: Find looks only at the entries of
   --  one bucket, not at the whole table.

   Table_Last       : Cursor := 0;
   --  The entries that the table has room for: Initialize maps them.
   Bucket_Bits      : Natural := 0;

   Mapped_Page_Last : Cursor := 0;
   Free_Count       : Natural := 0;
   Free_Head        : Cursor := 0;
   --  Entries up to Mapped_Page_Last that are free, in a list through
   --  Next_Page.

   Dirty_Page_Last : Natural := 0;

   Clock_Hand : Cursor := 0;
   --  The page that Sweep_Clean_Pages or Sweep_Changed_Pages last
   --  stopped at.

   function Bucket_Of
     (Process : Process_Id;
      Virtual : Rose.Addresses.Virtual_Page_Address)
      return Natural;
   --  The bucket of the page Virtual of Process.

   procedure Release_Entry (Position : Cursor);
   --  Take the page at Position out of its bucket, and make its entry
   --  free.

   procedure Sweep
     (Changed  : Boolean;
      Try_Page : not null access
        procedure (Position : Cursor;
                   Taken    : out Boolean);
      Position : out Cursor);
   --  Sweep_Clean_Pages if not Changed, else Sweep_Changed_Pages.

   procedure Drop_Deleted_Dirty_Pages;
   --  Take pages that have been deleted out of the dirty page list, so
   --  that the next checkpoint doesn't save them: their slots will be
   --  used again for other pages.

   ---------------
   -- Bucket_Of --
   ---------------

   function Bucket_Of
     (Process : Process_Id;
      Virtual : Rose.Addresses.Virtual_Page_Address)
      return Natural
   is
      use Rose.Words;
      Key : constant Word_32 :=
              (Word_32 (Virtual) * 16#9E37_79B1#)
              xor (Word_32 (Process) * 16#85EB_CA6B#);
   begin
      --  The top bits of a multiplicative hash are the well mixed ones.

      return Natural (Key / Word_32'(2) ** (32 - Bucket_Bits));
   end Bucket_Of;

   -----------------------
   -- Clear_Dirty_Pages --
   -----------------------

   procedure Clear_Dirty_Pages is
   begin
      for Position of Dirty_Pages (1 .. Dirty_Page_Last) loop
         Mapped_Pages (Position).Dirty := False;
      end loop;
      Dirty_Page_Last := 0;
   end Clear_Dirty_Pages;

   ------------
   -- Delete --
   ------------

   procedure Delete (Position : Cursor) is
      Was_Dirty : constant Boolean := Mapped_Pages (Position).Dirty;
   begin
      Release_Entry (Position);

      --  Only a dirty page is in the dirty page list. Dropping a clean
      --  page can happen during a checkpoint, which is going through the
      --  list (#973).

      if Was_Dirty then
         Drop_Deleted_Dirty_Pages;
      end if;
   end Delete;

   ----------------
   -- Delete_All --
   ----------------

   procedure Delete_All
     (Process : Process_Id)
   is
   begin
      for I in 1 .. Mapped_Page_Last loop
         declare
            Element : Mapped_Page_Record renames Mapped_Pages (I);
         begin
            if Element.Process = Process then
               declare
                  Cached, Last, Modified : Boolean;
               begin
                  --  A cached page stays in memory for the next process
                  --  that maps it, until Reclaim needs it. A shared page
                  --  stays while another process maps it. Its process
                  --  has already written it back if this is the last
                  --  mapping (Mem.Processes.Release_Shared_Segments).

                  if Mem.Shared_Pages.Is_Shared (Element.Phys) then
                     Mem.Shared_Pages.Release (Element.Phys, Last, Modified);
                     if Last then
                        Mem.Physical_Map.Deallocate_Page (Element.Phys);
                     end if;
                  else
                     Mem.Page_Cache.Release (Element.Phys, Cached);
                     if not Cached then
                        Mem.Physical_Map.Deallocate_Page (Element.Phys);
                     end if;
                  end if;
               end;
               Release_Entry (I);
            end if;
         end;
      end loop;

      Drop_Deleted_Dirty_Pages;
   end Delete_All;

   ----------------------
   -- Dirty_Page_Count --
   ----------------------

   function Dirty_Page_Count return Natural is
   begin
      return Dirty_Page_Last;
   end Dirty_Page_Count;

   ------------------------------
   -- Drop_Deleted_Dirty_Pages --
   ------------------------------

   procedure Drop_Deleted_Dirty_Pages is
      Kept : Natural := 0;
   begin
      for I in 1 .. Dirty_Page_Last loop
         if Mapped_Pages (Dirty_Pages (I)).Process /= 0 then
            Kept := Kept + 1;
            Dirty_Pages (Kept) := Dirty_Pages (I);
         end if;
      end loop;
      Dirty_Page_Last := Kept;
   end Drop_Deleted_Dirty_Pages;

   ----------
   -- Find --
   ----------

   function Find
     (Process    : Process_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address)
      return Cursor
   is
      use type Rose.Addresses.Virtual_Page_Address;
      Position : Cursor := Buckets (Bucket_Of (Process, Virtual));
   begin
      while Position /= 0 loop
         declare
            Element : Mapped_Page_Record renames Mapped_Pages (Position);
         begin
            if Element.Process = Process
              and then Element.Virt = Virtual
            then
               return Position;
            end if;
         end;
         Position := Next_Page (Position);
      end loop;
      return 0;
   end Find;

   ------------------
   -- Free_Entries --
   ------------------

   function Free_Entries return Natural is
   begin
      return Natural (Table_Last - Mapped_Page_Last) + Free_Count;
   end Free_Entries;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      Physical : constant Natural :=
                   Natural (Mem.Physical_Map.Available_Pages);
      Entries  : constant Natural :=
                   Natural'Min
                     (Max_Entries,
                      Natural'Max (Rose.Limits.Min_Mapped_Pages,
                                   2 * Physical));
      Success  : Boolean := True;

      procedure Map_Pages
        (Base  : System.Address;
         Bytes : Natural);
      --  Map pages of mem's own from Base, enough for Bytes.

      ---------------
      -- Map_Pages --
      ---------------

      procedure Map_Pages
        (Base  : System.Address;
         Bytes : Natural)
      is
         use System.Storage_Elements;
         Page  : Rose.Addresses.Physical_Page_Address;
      begin
         for I in 0 .. (Bytes + Rose.Limits.Page_Size - 1)
           / Rose.Limits.Page_Size - 1
         loop
            exit when not Success;
            Mem.Physical_Map.Allocate_Page (Page, Success);
            if Success then
               Mem.Calls.Load_Page
                 (Page,
                  Base + Storage_Offset (I * Rose.Limits.Page_Size));
            end if;
         end loop;
      end Map_Pages;

   begin
      Bucket_Bits := 12;
      while 2 ** Bucket_Bits < Entries / 2
        and then Bucket_Bits < Max_Bucket_Bits
      loop
         Bucket_Bits := Bucket_Bits + 1;
      end loop;

      Map_Pages (Mapped_Pages'Address, Entries * 8);
      Map_Pages (Next_Page'Address, Entries * 4);
      Map_Pages (Dirty_Pages'Address, Entries * 4);
      Map_Pages (Buckets'Address, 2 ** Bucket_Bits * 4);

      if not Success then
         Rose.Console_IO.Put_Line ("mem: no memory for the page table");
         return;
      end if;

      for Bucket of Buckets (0 .. 2 ** Bucket_Bits - 1) loop
         Bucket := 0;
      end loop;

      Table_Last := Cursor (Entries);

      Rose.Console_IO.Put ("mem: page table: ");
      Rose.Console_IO.Put (Entries);
      Rose.Console_IO.Put (" entries for ");
      Rose.Console_IO.Put (Physical);
      Rose.Console_IO.Put_Line (" pages");
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Process    : Process_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Physical   : Rose.Addresses.Physical_Page_Address;
      Mapped     : Boolean;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean;
      Persistent : Boolean)
   is
      Position : Cursor := 0;
      Bucket   : constant Natural := Bucket_Of (Process, Virtual);
   begin

      if Free_Head /= 0 then
         Position := Free_Head;
         Free_Head := Next_Page (Position);
         Free_Count := Free_Count - 1;
      else
         if Mapped_Page_Last = Table_Last then

            --  The page is mapped already, so it is now one that mem
            --  doesn't track, and a later fault on it will fail its
            --  process. Mem.Virtual_Map.Map frees pages before the table
            --  fills, so this is only when none could be freed. It goes
            --  to the kernel's log and not the console: Insert is on the
            --  path of a page fault, and the console calls a driver that
            --  may itself wait on a fault that only mem can handle
            --  (#591, as in #914).

            Rose.Trace.Event
              ("mem: too many mapped pages, process", Natural (Process));
            return;
         end if;

         Mapped_Page_Last := Mapped_Page_Last + 1;
         Position := Mapped_Page_Last;
      end if;

      Mapped_Pages (Position) :=
        Mapped_Page_Record'
          (Process    => Process,
           Virt       => Virtual,
           Phys       => Physical,
           Mapped     => Mapped,
           Readable   => Readable,
           Writable   => Writable,
           Executable => Executable,
           Persistent => Persistent,
           Dirty      => False);

      Next_Page (Position) := Buckets (Bucket);
      Buckets (Bucket) := Position;

   end Insert;

   --------------
   -- Is_Dirty --
   --------------

   function Is_Dirty (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Dirty;
   end Is_Dirty;

   -------------------
   -- Is_Executable --
   -------------------

   function Is_Executable (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Executable;
   end Is_Executable;

   ---------------
   -- Is_Mapped --
   ---------------

   function Is_Mapped (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Mapped;
   end Is_Mapped;

   -------------------
   -- Is_Persistent --
   -------------------

   function Is_Persistent (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Persistent;
   end Is_Persistent;

   -----------------
   -- Is_Readable --
   -----------------

   function Is_Readable (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Readable;
   end Is_Readable;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (Position : Cursor) return Boolean is
   begin
      return Mapped_Pages (Position).Writable;
   end Is_Writable;

   -------------------------
   -- Iterate_Dirty_Pages --
   -------------------------

   procedure Iterate_Dirty_Pages
     (Process : not null access procedure (Position : Cursor))
   is
   begin
      for I in 1 .. Dirty_Page_Last loop
         Process (Dirty_Pages (I));
      end loop;
   end Iterate_Dirty_Pages;

   ---------------------------
   -- Iterate_Process_Pages --
   ---------------------------

   procedure Iterate_Process_Pages
     (Process : Process_Id;
      Base    : Rose.Addresses.Virtual_Page_Address;
      Bound   : Rose.Addresses.Virtual_Page_Address;
      Action  : not null access procedure (Position : Cursor))
   is
      use type Rose.Addresses.Virtual_Page_Address;
   begin
      for I in 1 .. Mapped_Page_Last loop
         if Mapped_Pages (I).Process = Process
           and then Mapped_Pages (I).Virt >= Base
           and then Mapped_Pages (I).Virt < Bound
         then
            Action (I);
         end if;
      end loop;
   end Iterate_Process_Pages;

   ----------------------
   -- Physical_Address --
   ----------------------

   function Physical_Address
     (Position : Cursor) return Rose.Addresses.Physical_Page_Address
   is
   begin
      return Mapped_Pages (Position).Phys;
   end Physical_Address;

   -------------
   -- Process --
   -------------

   function Process
     (Position : Cursor)
      return Process_Id
   is
   begin
      return Mapped_Pages (Position).Process;
   end Process;

   -------------------
   -- Release_Entry --
   -------------------

   procedure Release_Entry (Position : Cursor) is
      Page     : Mapped_Page_Record renames Mapped_Pages (Position);
      Bucket   : constant Natural := Bucket_Of (Page.Process, Page.Virt);
      Previous : Cursor := 0;
      Current  : Cursor := Buckets (Bucket);
   begin
      if Page.Process = 0 then
         return;
      end if;

      while Current /= 0 and then Current /= Position loop
         Previous := Current;
         Current := Next_Page (Current);
      end loop;

      if Current = Position then
         if Previous = 0 then
            Buckets (Bucket) := Next_Page (Position);
         else
            Next_Page (Previous) := Next_Page (Position);
         end if;
      end if;

      Page.Process := 0;
      Next_Page (Position) := Free_Head;
      Free_Head := Position;
      Free_Count := Free_Count + 1;
   end Release_Entry;

   --------------------------
   -- Set_Physical_Address --
   --------------------------

   procedure Set_Physical_Address
     (Position : Cursor; Phys : Rose.Addresses.Physical_Page_Address)
   is
   begin
      Mapped_Pages (Position).Phys := Phys;
   end Set_Physical_Address;

   -------------------
   -- Set_Read_Only --
   -------------------

   procedure Set_Read_Only (Position : Cursor) is
   begin
      Mapped_Pages (Position).Writable := False;
   end Set_Read_Only;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (Position : Cursor) is
      Page : Mapped_Page_Record renames Mapped_Pages (Position);
   begin
      Page.Writable := True;
      if Page.Persistent and then not Page.Dirty then
         if Dirty_Page_Last >= Rose.Limits.Max_Dirty_Pages then

            --  To the kernel's log, not the console, as for Insert. A
            --  trace event carries 32 characters.

            Rose.Trace.Event
              ("mem: dirty limit passed, pid ", Natural (Page.Process));
         end if;
         Dirty_Page_Last := Dirty_Page_Last + 1;
         Dirty_Pages (Dirty_Page_Last) := Position;
      end if;
      Page.Dirty := True;

      Mem.Calls.Map
        (Process    =>
           Mem.Processes.Process_Object_Id (Page.Process),
         Physical   => Page.Phys,
         Virtual    => Page.Virt,
         Readable   => Page.Readable,
         Writeable  => Page.Writable,
         Executable => Page.Executable);
   end Set_Writable;

   -----------
   -- Sweep --
   -----------

   procedure Sweep
     (Changed  : Boolean;
      Try_Page : not null access
        procedure (Position : Cursor;
                   Taken    : out Boolean);
      Position : out Cursor)
   is
   begin
      Position := 0;

      for Step in 1 .. Mapped_Page_Last loop
         Clock_Hand :=
           (if Clock_Hand >= Mapped_Page_Last then 1 else Clock_Hand + 1);

         declare
            Page  : Mapped_Page_Record renames Mapped_Pages (Clock_Hand);
            Taken : Boolean;
         begin
            if Page.Process /= 0
              and then Page.Mapped
              and then Page.Persistent
              and then
                (if Changed then Page.Dirty
                 else not Page.Dirty and then not Page.Writable)
            then
               Try_Page (Clock_Hand, Taken);
               if Taken then
                  Position := Clock_Hand;
                  return;
               end if;
            end if;
         end;
      end loop;
   end Sweep;

   -------------------------
   -- Sweep_Changed_Pages --
   -------------------------

   procedure Sweep_Changed_Pages
     (Try_Page : not null access
        procedure (Position : Cursor;
                   Taken    : out Boolean);
      Position : out Cursor)
   is
   begin
      Sweep (True, Try_Page, Position);
   end Sweep_Changed_Pages;

   -----------------------
   -- Sweep_Clean_Pages --
   -----------------------

   procedure Sweep_Clean_Pages
     (Try_Page : not null access
        procedure (Position : Cursor;
                   Taken    : out Boolean);
      Position : out Cursor)
   is
   begin
      Sweep (False, Try_Page, Position);
   end Sweep_Clean_Pages;

   ---------------------
   -- Virtual_Address --
   ---------------------

   function Virtual_Address
     (Position : Cursor)
      return Rose.Addresses.Virtual_Page_Address
   is
   begin
      return Mapped_Pages (Position).Virt;
   end Virtual_Address;

   ----------------------------
   -- Would_Pass_Dirty_Limit --
   ----------------------------

   function Would_Pass_Dirty_Limit (Position : Cursor) return Boolean is
      Page : Mapped_Page_Record renames Mapped_Pages (Position);
   begin
      return Page.Persistent
        and then not Page.Dirty
        and then Dirty_Page_Last >= Rose.Limits.Max_Dirty_Pages;
   end Would_Pass_Dirty_Limit;

end Mem.Page_Table;
