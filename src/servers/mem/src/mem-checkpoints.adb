with Rose.Addresses;
with Rose.Limits;
with Rose.Words;

with Mem.Page_Table;
with Mem.Virtual_Map;

package body Mem.Checkpoints is

   Checkpoint_Header_Magic : constant := 16#7E6A_F178_79CB_E048#;

   type Checkpoint_Header_Flags is new Rose.Words.Word_32;

   Is_Last_Header_Page     : constant Checkpoint_Header_Flags := 16#0001#;

   type Checkpoint_Page_Reference is
      record
         Process : Process_Id := 0;
         Page    : Rose.Addresses.Virtual_Page_Address := 0;
      end record
     with Pack, Size => 40;

   Max_Page_Objects_Per_Header : constant := (4096 - 16) / 5;

   type Header_Page_Object_Array is
     array (Rose.Words.Word_32 range 1 .. Max_Page_Objects_Per_Header)
     of Checkpoint_Page_Reference;

   type Checkpoint_Header_Page is
      record
         Magic : Rose.Words.Word_64 := Checkpoint_Header_Magic;
         Flags : Checkpoint_Header_Flags := 0;
         Count : Rose.Words.Word_32 := 0;
         Pages : Header_Page_Object_Array;
      end record
   with Pack, Size => Rose.Limits.Page_Size * 8;

   Checkpoint_Header       : Checkpoint_Header_Page;

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
         if Checkpoint_Header.Count = Max_Page_Objects_Per_Header then
            Append (Checkpoint_Header'Address);
            Reset_Header;
         end if;

         Checkpoint_Header.Count := Checkpoint_Header.Count + 1;
         Checkpoint_Header.Pages (Checkpoint_Header.Count) :=
           Checkpoint_Page_Reference'
             (Process => Mem.Page_Table.Process (Position),
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

      Reset_Header;

      Mem.Page_Table.Iterate_Dirty_Pages (Update_Header'Access);

      Checkpoint_Header.Flags :=
        Checkpoint_Header.Flags or Is_Last_Header_Page;
      Append (Checkpoint_Header'Address);
      Mem.Page_Table.Iterate_Dirty_Pages (Write_Page'Access);
      Mem.Page_Table.Clear_Dirty_Pages;
   end Checkpoint;

end Mem.Checkpoints;
