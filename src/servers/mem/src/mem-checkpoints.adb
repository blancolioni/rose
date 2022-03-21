with Rose.Addresses;
with Rose.Console_IO;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Mem.Processes;
with Mem.Virtual_Map;

package body Mem.Checkpoints is

   Checkpoint_Header_Magic : constant := 16#7E6A_F178_79CB_E048#;

   type Checkpoint_Header_Flags is new Rose.Words.Word_32;

   Is_Last_Header_Page     : constant Checkpoint_Header_Flags := 16#0001#;

   Max_Page_Objects_Per_Header : constant := (4096 - 16) / 8;

   type Header_Page_Object_Array is
     array (Rose.Words.Word_32 range 1 .. Max_Page_Objects_Per_Header)
     of Rose.Objects.Object_Id;


   type Checkpoint_Header_Page is
      record
         Magic : Rose.Words.Word_64 := Checkpoint_Header_Magic;
         Flags : Checkpoint_Header_Flags := 0;
         Count : Rose.Words.Word_32 := 0;
         Oids  : Header_Page_Object_Array := (others => 0);
      end record
   with Pack, Size => Rose.Limits.Page_Size * 8;

   Checkpoint_Header       : Checkpoint_Header_Page;

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint
     (Append : not null access procedure (Buffer : System.Address))
   is
      procedure Update_Header (Page : Rose.Objects.Page_Object_Id);
      procedure Write_Page (Page : Rose.Addresses.Physical_Page_Address);

      -------------------
      -- Update_Header --
      -------------------

      procedure Update_Header (Page : Rose.Objects.Page_Object_Id) is
         use type Rose.Words.Word_32;
      begin
         if Checkpoint_Header.Count = Max_Page_Objects_Per_Header then
            Append (Checkpoint_Header'Address);
            Checkpoint_Header.Flags := 0;
            Checkpoint_Header.Count := 0;
            for Oid of Checkpoint_Header.Oids loop
               Oid := 0;
            end loop;
         end if;

         Checkpoint_Header.Count := Checkpoint_Header.Count + 1;
         Checkpoint_Header.Oids (Checkpoint_Header.Count) := Page;
      end Update_Header;

      ----------------
      -- Write_Page --
      ----------------

      procedure Write_Page (Page : Rose.Addresses.Physical_Page_Address) is
      begin
         Mem.Virtual_Map.With_Page (Page, Append);
      end Write_Page;

   begin
      Mem.Processes.Iterate_Dirty_Page_Ids (Update_Header'Access);
      Checkpoint_Header.Flags :=
        Checkpoint_Header.Flags or Is_Last_Header_Page;
      Append (Checkpoint_Header'Address);
      Rose.Console_IO.Put ("mem: writing ");
      Rose.Console_IO.Put (Natural (Checkpoint_Header.Count));
      Rose.Console_IO.Put (" dirty pages");
      Rose.Console_IO.New_Line;

      Mem.Processes.Iterate_Dirty_Pages (Write_Page'Access);

      Mem.Processes.Clear_Dirty_Pages;
      Rose.Console_IO.Put_Line ("done");
   end Checkpoint;

end Mem.Checkpoints;
