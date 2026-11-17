with System.Storage_Elements;

with Rose.Addresses;
with Rose.Checkpoints;
with Rose.Console_IO;
with Rose.Limits;

with Mem.Page_Log;
with Mem.Page_Table;
with Mem.Shared_Pages;

package body Mem.Log_Index is

   use Rose.Words;
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

   subtype Page_Buffer is
     System.Storage_Elements.Storage_Array (1 .. Rose.Limits.Page_Size);

   Leaf_Buffer      : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;
   Directory_Buffer : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;
   Header_Buffer    : Page_Buffer
     with Alignment => Rose.Limits.Page_Size;

   Leaf      : Rose.Checkpoints.Index_Page
     with Import, Address => Leaf_Buffer'Address;
   Directory : Rose.Checkpoints.Index_Page
     with Import, Address => Directory_Buffer'Address;
   Header    : Rose.Checkpoints.Index_Header_Page
     with Import, Address => Header_Buffer'Address;

   procedure Fill_Leaf
     (Key         : Integer;
      Batch_First : Word_32);
   --  Set the entries of Leaf for the pages of the batch in leaf Key.

   function Key_Of
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Integer;

   function Next_Key (After : Integer) return Integer;
   --  The smallest key after After of a leaf with a page in the batch, or
   --  No_Key if there is none.

   function Process_Of (Key : Integer) return Process_Id;

   procedure Read_Page
     (Log_Page : Word_32;
      Buffer   : System.Address);
   --  Read log page Log_Page into the page buffer at Buffer, or fill it
   --  with zeroes if Log_Page is 0.

   function Slot_Of (Key : Integer) return Natural;

   --------------------
   -- End_Checkpoint --
   --------------------

   procedure End_Checkpoint (Committed : Boolean) is
   begin
      if Committed then
         Directories := New_Directories;
      else
         New_Directories := Directories;
      end if;
   end End_Checkpoint;

   ---------------
   -- Fill_Leaf --
   ---------------

   procedure Fill_Leaf
     (Key         : Integer;
      Batch_First : Word_32)
   is
      Index : Word_32 := 0;

      procedure Check (Position : Mem.Page_Table.Cursor);

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
               Leaf.Entries (Natural (Page mod Entries_Per_Page)) :=
                 Batch_First + Index;
            end if;
            Index := Index + 1;
         end if;
      end Check;

   begin
      Mem.Page_Table.Iterate_Dirty_Pages (Check'Access);
   end Fill_Leaf;

   ------------
   -- Forget --
   ------------

   procedure Forget (Process : Process_Id) is
   begin
      if Process in Real_Process_Id then
         Directories (Process) := 0;
         New_Directories (Process) := 0;
      end if;
   end Forget;

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
   -- Next_Key --
   --------------

   function Next_Key (After : Integer) return Integer is
      Result : Integer := No_Key;

      procedure Check (Position : Mem.Page_Table.Cursor);

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

   begin
      Mem.Page_Table.Iterate_Dirty_Pages (Check'Access);
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
   begin
      if Log_Page = 0 then
         Page := (others => 0);
      else
         Mem.Page_Log.Read (Log_Page, Buffer);
      end if;
   end Read_Page;

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

   -------------
   -- Slot_Of --
   -------------

   function Slot_Of (Key : Integer) return Natural is
   begin
      return Key mod Entries_Per_Page;
   end Slot_Of;

   -----------------
   -- Write_Index --
   -----------------

   procedure Write_Index
     (Batch_First : Rose.Words.Word_32;
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
         Fill_Leaf (Key, Batch_First);
         Append (Leaf_Buffer'Address);
         Directory.Entries (Slot_Of (Key)) := Log_Page;

         Next := Next_Key (Key);
         if Next = No_Key or else Process_Of (Next) /= Process then
            Append (Directory_Buffer'Address);
            New_Directories (Process) := Log_Page;
         end if;
         Key := Next;
      end loop;
   end Write_Index;

end Mem.Log_Index;
