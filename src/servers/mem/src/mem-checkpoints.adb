with System.Storage_Elements;

with Rose.Addresses;
with Rose.Checkpoints;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

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
     (Append : not null access procedure (Buffer : System.Address));
   --  Append the memory server's table pages.

   procedure Write_Shared_Pages
     (Append : not null access procedure (Buffer : System.Address));
   --  Append the shared header pages, and the shared pages they list as
   --  saved (ADR 0025).

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint
     (Append : not null access procedure (Buffer : System.Address))
   is

      procedure Reset_Header;

      procedure Update_Header (Position : Mem.Page_Table.Cursor);
      procedure Write_Page (Position : Mem.Page_Table.Cursor);

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
         use type Rose.Words.Word_32;
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
            Mem.Virtual_Map.With_Page (Physical, Append);
         end if;
      end Write_Page;

      Has_Shared : constant Boolean := Mem.Shared_Pages.Has_Checkpoint_Pages;

   begin

      Write_Tables (Append);

      Reset_Header;

      Mem.Page_Table.Iterate_Dirty_Pages (Update_Header'Access);

      Checkpoint_Header.Flags :=
        Checkpoint_Header.Flags or Is_Last_Header_Page
        or (if Has_Shared then Has_Shared_Pages else 0);
      Append (Checkpoint_Header'Address);
      Mem.Page_Table.Iterate_Dirty_Pages (Write_Page'Access);
      Mem.Page_Table.Clear_Dirty_Pages;

      if Has_Shared then
         Write_Shared_Pages (Append);
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
      Mem.Shared_Pages.Clear_Checkpoint_Pages;
   end Write_Shared_Pages;

   ------------------
   -- Write_Tables --
   ------------------

   procedure Write_Tables
     (Append : not null access procedure (Buffer : System.Address))
   is
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
      Append (Checkpoint_Header'Address);

      Mem.Processes.Iterate_Saved_Processes (Write_Process'Access);
   end Write_Tables;

end Mem.Checkpoints;
