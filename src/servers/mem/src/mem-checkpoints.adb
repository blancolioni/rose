with System.Storage_Elements;

with Rose.Addresses;
with Rose.Checkpoints;
with Rose.Console_IO;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Mem.Log_Index;
with Mem.Page_Log;
with Mem.Page_Table;
with Mem.Processes;
with Mem.Shared_Pages;
with Mem.Virtual_Map;

package body Mem.Checkpoints is

   use Rose.Checkpoints;

   Checkpoint_Header       : Header_Page;

   --  The table pages and the shared header pages are built in the same
   --  page as the header pages, since each is appended before the next
   --  is built.

   Table_Page    : System.Storage_Elements.Storage_Array
                     (1 .. Rose.Limits.Page_Size)
     with Import, Address => Checkpoint_Header'Address;
   Table_Header  : Mem_Table_Header
     with Import, Address => Checkpoint_Header'Address;
   Process_Entry : Mem_Process_Record
     with Import, Address => Checkpoint_Header'Address;
   Shared_Header : Shared_Header_Page
     with Import, Address => Checkpoint_Header'Address;

   procedure Write_Tables
     (Append     : not null access procedure (Buffer : System.Address);
      Has_Shared : Boolean);
   --  Append the memory server's table pages.  Has_Shared says that the
   --  pages of shared segments follow them.

   procedure Write_Shared_Pages
     (Append : not null access procedure (Buffer : System.Address));
   --  Append the shared header pages, and the shared pages they list as
   --  saved (ADR 0025).

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint is

      use type Rose.Words.Word_32;

      Failed      : Boolean := False;
      --  An append failed, or the batch's pages weren't at consecutive
      --  log pages.

      Batch_First : Rose.Words.Word_32 := 0;
      Batch_Count : Rose.Words.Word_32 := 0;
      --  The log page of the batch's first page, and the number of its
      --  pages appended so far.

      procedure Append (Buffer : System.Address);

      procedure Append_Batch_Page (Buffer : System.Address);
      --  Append a page of the batch, which must go at the log page after
      --  the one before, since Mem.Log_Index finds each page's log page
      --  by its place in the batch.

      procedure Reset_Header;

      procedure Update_Header (Position : Mem.Page_Table.Cursor);
      procedure Write_Page (Position : Mem.Page_Table.Cursor);

      ------------
      -- Append --
      ------------

      procedure Append (Buffer : System.Address) is
      begin
         if not Failed and then Mem.Page_Log.Append (Buffer) = 0 then
            Failed := True;
         end if;
      end Append;

      -----------------------
      -- Append_Batch_Page --
      -----------------------

      procedure Append_Batch_Page (Buffer : System.Address) is
         Log_Page : Rose.Words.Word_32;
      begin
         if Failed then
            return;
         end if;

         Log_Page := Mem.Page_Log.Append (Buffer);

         if Log_Page = 0 then
            Failed := True;
         elsif Batch_Count = 0 then
            Batch_First := Log_Page;
         elsif Log_Page /= Batch_First + Batch_Count then
            Failed := True;
         end if;

         Batch_Count := Batch_Count + 1;
      end Append_Batch_Page;

      ------------------
      -- Reset_Header --
      ------------------

      procedure Reset_Header is
      begin
         --  The table pages were built in this page too.
         Checkpoint_Header.Magic := Header_Page_Magic;
         Checkpoint_Header.Flags := 0;
         Checkpoint_Header.Count := 0;
         for Element of Checkpoint_Header.Pages loop
            Element := (others => <>);
         end loop;
      end Reset_Header;

      -------------------
      -- Update_Header --
      -------------------

      procedure Update_Header (Position : Mem.Page_Table.Cursor) is
      begin
         --  A shared page is saved once, by its store page, however many
         --  processes have written it.

         if Mem.Shared_Pages.Is_Shared
           (Mem.Page_Table.Physical_Address (Position))
         then
            return;
         end if;

         if Checkpoint_Header.Count = Max_Page_References then
            Append (Checkpoint_Header'Address);
            Reset_Header;
         end if;

         Checkpoint_Header.Count := Checkpoint_Header.Count + 1;
         Checkpoint_Header.Pages (Checkpoint_Header.Count) :=
           Page_Reference'
             (Process =>
                Rose.Words.Word_16 (Mem.Page_Table.Process (Position)),
              Page    => Mem.Page_Table.Virtual_Address (Position));

      end Update_Header;

      ----------------
      -- Write_Page --
      ----------------

      procedure Write_Page (Position : Mem.Page_Table.Cursor) is
         Physical : constant Rose.Addresses.Physical_Page_Address :=
                      Mem.Page_Table.Physical_Address (Position);
      begin
         Mem.Page_Table.Set_Read_Only (Position);
         if not Mem.Shared_Pages.Is_Shared (Physical) then
            Mem.Virtual_Map.With_Page (Physical, Append_Batch_Page'Access);
         end if;
      end Write_Page;

      Has_Shared : constant Boolean := Mem.Shared_Pages.Has_Checkpoint_Pages;
      Index_Written : Boolean;

   begin

      if not Mem.Page_Log.Is_Available then
         Rose.Console_IO.Put_Line
           ("mem: checkpoint: no Page_Log; nothing saved");
         return;
      end if;

      --  The batch of changed pages comes first, then the index pages
      --  that name them, then the tables, which name the processes that
      --  the header pages refer to by the memory server's ids, and their
      --  index directories (ADR 0028, section 4).

      Reset_Header;

      Mem.Page_Table.Iterate_Dirty_Pages (Update_Header'Access);

      Checkpoint_Header.Flags :=
        Checkpoint_Header.Flags or Is_Last_Header_Page;
      Append (Checkpoint_Header'Address);
      Mem.Page_Table.Iterate_Dirty_Pages (Write_Page'Access);

      if not Failed then
         Mem.Log_Index.Write_Index (Batch_First, Index_Written);
         Failed := not Index_Written;
      end if;

      if not Failed then
         Write_Tables (Append'Access, Has_Shared);
      end if;

      if not Failed and then Has_Shared then
         Write_Shared_Pages (Append'Access);
      end if;

      --  A failed checkpoint's pages are dropped from the log, so the
      --  pages it saved are still dirty, and the index it wrote is not
      --  used (ADR 0028, section 11). The log server has already failed
      --  the checkpoint if an append did, but not if the batch's pages
      --  went astray.
      --
      --  There is no message here.  The console may be waiting on a
      --  process that checkpoint mode has blocked, and the checkpoint is
      --  waiting on mem (#971).  The log server reports the failure when
      --  the checkpoint commits.

      if Failed then
         Mem.Page_Log.Fail_Checkpoint;
         Mem.Log_Index.End_Checkpoint (Committed => False);
      else
         Mem.Log_Index.End_Checkpoint (Committed => True);
         Mem.Page_Table.Clear_Dirty_Pages;
         if Has_Shared then
            Mem.Shared_Pages.Clear_Checkpoint_Pages;
         end if;
      end if;
   end Checkpoint;

   ------------------------
   -- Write_Shared_Pages --
   ------------------------

   procedure Write_Shared_Pages
     (Append : not null access procedure (Buffer : System.Address))
   is
      use type Rose.Words.Word_32;

      procedure Reset_Header;

      procedure Add_Reference
        (Page  : Rose.Objects.Page_Object_Id;
         Flags : Rose.Words.Word_32);

      procedure Add_Released (Page : Rose.Objects.Page_Object_Id);

      procedure Add_Saved
        (Page     : Rose.Objects.Page_Object_Id;
         Physical : Rose.Addresses.Physical_Page_Address);

      procedure Write_Page
        (Page     : Rose.Objects.Page_Object_Id;
         Physical : Rose.Addresses.Physical_Page_Address);

      -------------------
      -- Add_Reference --
      -------------------

      procedure Add_Reference
        (Page  : Rose.Objects.Page_Object_Id;
         Flags : Rose.Words.Word_32)
      is
      begin
         if Shared_Header.Count = Max_Shared_References then
            Append (Shared_Header'Address);
            Reset_Header;
         end if;

         Shared_Header.Count := Shared_Header.Count + 1;
         Shared_Header.Pages (Shared_Header.Count) :=
           (Page => Rose.Words.Word_64 (Page), Flags => Flags);
      end Add_Reference;

      ------------------
      -- Add_Released --
      ------------------

      procedure Add_Released (Page : Rose.Objects.Page_Object_Id) is
      begin
         Add_Reference (Page, Shared_Page_Released);
      end Add_Released;

      ---------------
      -- Add_Saved --
      ---------------

      procedure Add_Saved
        (Page     : Rose.Objects.Page_Object_Id;
         Physical : Rose.Addresses.Physical_Page_Address)
      is
         pragma Unreferenced (Physical);
      begin
         Add_Reference (Page, Shared_Page_Saved);
      end Add_Saved;

      ------------------
      -- Reset_Header --
      ------------------

      procedure Reset_Header is
      begin
         Table_Page := (others => 0);
         Shared_Header.Magic := Shared_Header_Magic;
      end Reset_Header;

      ----------------
      -- Write_Page --
      ----------------

      procedure Write_Page
        (Page     : Rose.Objects.Page_Object_Id;
         Physical : Rose.Addresses.Physical_Page_Address)
      is
         pragma Unreferenced (Page);
      begin
         Mem.Virtual_Map.With_Page (Physical, Append);
      end Write_Page;

   begin
      Reset_Header;

      --  The released pages first: a page that was released and then
      --  mapped and written again is saved too, and a restore takes the
      --  last that it reads.

      Mem.Shared_Pages.Iterate_Released (Add_Released'Access);
      Mem.Shared_Pages.Iterate_Unsaved (Add_Saved'Access);

      Shared_Header.Flags := Is_Last_Header_Page;
      Append (Shared_Header'Address);

      Mem.Shared_Pages.Iterate_Unsaved (Write_Page'Access);
   end Write_Shared_Pages;

   ------------------
   -- Write_Tables --
   ------------------

   procedure Write_Tables
     (Append     : not null access procedure (Buffer : System.Address);
      Has_Shared : Boolean)
   is
      use type Rose.Words.Word_32;
      Count : Natural := 0;

      procedure Count_Process (Pid : Mem.Process_Id);
      procedure Write_Process (Pid : Mem.Process_Id);

      -------------------
      -- Count_Process --
      -------------------

      procedure Count_Process (Pid : Mem.Process_Id) is
         pragma Unreferenced (Pid);
      begin
         Count := Count + 1;
      end Count_Process;

      -------------------
      -- Write_Process --
      -------------------

      procedure Write_Process (Pid : Mem.Process_Id) is
      begin
         Table_Page := (others => 0);
         Mem.Processes.Save_Process (Pid, Process_Entry);
         Append (Checkpoint_Header'Address);
      end Write_Process;

   begin
      Mem.Processes.Iterate_Saved_Processes (Count_Process'Access);

      Table_Page := (others => 0);
      Table_Header.Magic := Mem_Table_Magic;
      Table_Header.Version := Mem_Table_Version;
      Table_Header.Page_Count := Rose.Words.Word_32 (Count + 1);
      Table_Header.Process_Count := Rose.Words.Word_32 (Count);
      Table_Header.Flags :=
        Mem_Has_Index
          or (if Has_Shared then Mem_Has_Shared_Pages else 0);
      Append (Checkpoint_Header'Address);

      Mem.Processes.Iterate_Saved_Processes (Write_Process'Access);
   end Write_Tables;

end Mem.Checkpoints;
