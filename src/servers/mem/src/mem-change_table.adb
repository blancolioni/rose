package body Mem.Change_Table is

   use Rose.Words;
   use type Rose.Addresses.Virtual_Page_Address;

   --  Open addressing, with linear probing. A slot whose Log_Page is 0 is
   --  empty, since log page 0 is the log's header.

   type Slot_Index is mod Capacity;

   type Change_Record is
      record
         Process  : Process_Id := 0;
         Page     : Rose.Addresses.Virtual_Page_Address := 0;
         Log_Page : Word_32 := 0;
      end record;

   Table      : array (Slot_Index) of Change_Record;
   Used_Count : Natural := 0;

   function Home
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Slot_Index;
   --  Where the search for Page of Process starts.

   function Search
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Slot_Index;
   --  The slot that holds Page of Process, or the empty slot where the
   --  search for it ended.

   procedure Remove (Slot : Slot_Index);
   --  Empty Slot, and move later entries back so that every entry can
   --  still be found from its home slot.

   ---------
   -- Add --
   ---------

   procedure Add
     (Process  : Process_Id;
      Page     : Rose.Addresses.Virtual_Page_Address;
      Log_Page : Rose.Words.Word_32)
   is
      Slot : constant Slot_Index := Search (Process, Page);
   begin
      if Table (Slot).Log_Page = 0 then
         Used_Count := Used_Count + 1;
      end if;
      Table (Slot) := (Process, Page, Log_Page);
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      if Used_Count > 0 then
         --  Entry by entry: an aggregate for the whole table would be
         --  built on mem's stack.

         for Item of Table loop
            Item := (others => <>);
         end loop;
         Used_Count := 0;
      end if;
   end Clear;

   -----------
   -- Count --
   -----------

   function Count return Natural is
   begin
      return Used_Count;
   end Count;

   ----------
   -- Find --
   ----------

   function Find
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Rose.Words.Word_32
   is
   begin
      if Used_Count = 0 then
         return 0;
      end if;
      return Table (Search (Process, Page)).Log_Page;
   end Find;

   ------------
   -- Forget --
   ------------

   procedure Forget (Process : Process_Id) is
      Slot : Slot_Index := 0;
      Seen : Natural := 0;
   begin
      --  Remove moves a later entry into the slot it empties, so the slot
      --  is looked at again. Each step either removes an entry or moves
      --  on, so this ends after at most twice the table's size.

      while Used_Count > 0 and then Seen < Capacity loop
         if Table (Slot).Log_Page /= 0
           and then Table (Slot).Process = Process
         then
            Remove (Slot);
         else
            Slot := Slot + 1;
            Seen := Seen + 1;
         end if;
      end loop;
   end Forget;

   ----------
   -- Home --
   ----------

   function Home
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Slot_Index
   is
      Key : constant Word_32 :=
              Word_32 (Process) * 16#9E37_79B1#
              + Word_32 (Page) * 16#85EB_CA6B#;
   begin
      return Slot_Index (Key / 2 ** 16 mod Capacity);
   end Home;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access
        procedure (Owner    : Process_Id;
                   Page     : Rose.Addresses.Virtual_Page_Address;
                   Log_Page : Rose.Words.Word_32))
   is
   begin
      if Used_Count = 0 then
         return;
      end if;

      for Item of Table loop
         if Item.Log_Page /= 0 then
            Process (Item.Process, Item.Page, Item.Log_Page);
         end if;
      end loop;
   end Iterate;

   ------------
   -- Remove --
   ------------

   procedure Remove (Slot : Slot_Index) is
      Hole : Slot_Index := Slot;
      Next : Slot_Index := Slot + 1;
   begin
      Table (Hole) := (others => <>);
      Used_Count := Used_Count - 1;

      --  An entry after the hole moves into it if its home is not between
      --  the hole and where it is now, going round the table.

      while Table (Next).Log_Page /= 0 loop
         declare
            Item_Home : constant Slot_Index :=
                          Home (Table (Next).Process, Table (Next).Page);
         begin
            if Next - Item_Home >= Next - Hole then
               Table (Hole) := Table (Next);
               Table (Next) := (others => <>);
               Hole := Next;
            end if;
         end;
         Next := Next + 1;
      end loop;
   end Remove;

   ----------
   -- Room --
   ----------

   function Room return Natural is
   begin
      return Capacity - 1 - Used_Count;
   end Room;

   ------------
   -- Search --
   ------------

   function Search
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Slot_Index
   is
      Slot : Slot_Index := Home (Process, Page);
   begin
      while Table (Slot).Log_Page /= 0
        and then (Table (Slot).Process /= Process
                  or else Table (Slot).Page /= Page)
      loop
         Slot := Slot + 1;
      end loop;
      return Slot;
   end Search;

end Mem.Change_Table;
