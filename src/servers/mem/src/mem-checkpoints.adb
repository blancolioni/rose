with System.Storage_Elements;

with Rose.Checkpoints;
with Rose.Limits;
with Rose.Words;

with Mem.Page_Table;
with Mem.Processes;
with Mem.Virtual_Map;

package body Mem.Checkpoints is

   use Rose.Checkpoints;

   Checkpoint_Header       : Header_Page;

   --  The table pages are built in the same page as the header pages,
   --  since they are appended before the first header page.

   Table_Page    : System.Storage_Elements.Storage_Array
                     (1 .. Rose.Limits.Page_Size)
     with Import, Address => Checkpoint_Header'Address;
   Table_Header  : Mem_Table_Header
     with Import, Address => Checkpoint_Header'Address;
   Process_Entry : Mem_Process_Record
     with Import, Address => Checkpoint_Header'Address;

   procedure Write_Tables
     (Append : not null access procedure (Buffer : System.Address));
   --  Append the memory server's table pages.

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
      begin
         Mem.Page_Table.Set_Read_Only (Position);
         Mem.Virtual_Map.With_Page
           (Mem.Page_Table.Physical_Address (Position), Append);
      end Write_Page;

   begin

      Write_Tables (Append);

      Reset_Header;

      Mem.Page_Table.Iterate_Dirty_Pages (Update_Header'Access);

      Checkpoint_Header.Flags :=
        Checkpoint_Header.Flags or Is_Last_Header_Page;
      Append (Checkpoint_Header'Address);
      Mem.Page_Table.Iterate_Dirty_Pages (Write_Page'Access);
      Mem.Page_Table.Clear_Dirty_Pages;
   end Checkpoint;

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
