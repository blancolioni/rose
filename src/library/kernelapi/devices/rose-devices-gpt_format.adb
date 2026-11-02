package body Rose.Devices.GPT_Format
  with SPARK_Mode
is

   use System.Storage_Elements;
   use Rose.Words;

   pragma Assertion_Policy (Ignore);
   --  The loop invariants and assertions are for the proof, and the boot
   --  servers' run-time system has no System.Assertions.

   procedure Put_32
     (Bytes  : in out Storage_Array;
      Offset : Storage_Offset;
      Value  : Word_32)
   with Pre => Fits (Bytes, Offset, 4);

   procedure Put_64
     (Bytes  : in out Storage_Array;
      Offset : Storage_Offset;
      Value  : Word_64)
   with Pre => Fits (Bytes, Offset, 8);

   function Get_32
     (Bytes  : Storage_Array;
      Offset : Storage_Offset)
      return Word_32
   with Pre => Fits (Bytes, Offset, 4);

   function Get_64
     (Bytes  : Storage_Array;
      Offset : Storage_Offset)
      return Word_64
   with Pre => Fits (Bytes, Offset, 8);

   --  The offsets of the fields of the header, which is 92 bytes, and of
   --  an entry, which is 128.

   ---------
   -- Add --
   ---------

   procedure Add
     (Table        : in out Partition_Table;
      Item         : Partition_Entry;
      First_Usable : Word_64;
      Last_Usable  : Word_64;
      Success      : out Boolean)
   is
   begin
      Success := False;

      if Table.Count = Max_Partitions
        or else not Valid_Entry (Item, First_Usable, Last_Usable)
      then
         return;
      end if;

      for I in 0 .. Table.Count - 1 loop
         pragma Loop_Invariant
           (for all K in 0 .. I - 1 => Disjoint (Table.Parts (K), Item));

         if not Disjoint (Table.Parts (I), Item) then
            return;
         end if;
      end loop;

      pragma Assert
        (for all K in 0 .. Table.Count - 1 =>
           Disjoint (Table.Parts (K), Item));

      Table.Parts (Table.Count) := Item;
      Table.Count := Table.Count + 1;
      Success := True;
   end Add;

   ------------------
   -- Decode_Entry --
   ------------------

   function Decode_Entry
     (Bytes  : Storage_Array;
      Offset : Storage_Offset)
      return Partition_Entry
   is
      Result : Partition_Entry;
   begin
      Result.Partition_Type_Low := Get_64 (Bytes, Offset);
      Result.Partition_Type_High := Get_64 (Bytes, Offset + 8);
      Result.Partition_Id_Low := Get_64 (Bytes, Offset + 16);
      Result.Partition_Id_High := Get_64 (Bytes, Offset + 24);
      Result.First_LBA := Get_64 (Bytes, Offset + 32);
      Result.Last_LBA := Get_64 (Bytes, Offset + 40);
      Result.Flags := Get_64 (Bytes, Offset + 48);

      for I in Result.Name'Range loop
         Result.Name (I) :=
           Character'Val
             (Natural (Bytes (Offset + 56 + Storage_Offset (I - 1))));
      end loop;

      return Result;
   end Decode_Entry;

   -------------------
   -- Decode_Header --
   -------------------

   function Decode_Header (Bytes : Storage_Array) return Header is
   begin
      return
        (Magic                 => Get_64 (Bytes, 0),
         Revision              => Get_32 (Bytes, 8),
         Header_Size           => Get_32 (Bytes, 12),
         Header_CRC            => Get_32 (Bytes, 16),
         Reserved_Zero         => Get_32 (Bytes, 20),
         Current_LBA           => Get_64 (Bytes, 24),
         Backup_LBA            => Get_64 (Bytes, 32),
         First_Usable_LBA      => Get_64 (Bytes, 40),
         Last_Usable_LBA       => Get_64 (Bytes, 48),
         Disk_GUID_Lo          => Get_64 (Bytes, 56),
         Disk_GUID_Hi          => Get_64 (Bytes, 64),
         Start_Partition_LBA   => Get_64 (Bytes, 72),
         Partition_Entry_Count => Get_32 (Bytes, 80),
         Partition_Entry_Size  => Get_32 (Bytes, 84),
         Partition_Array_CRC   => Get_32 (Bytes, 88));
   end Decode_Header;

   ------------------
   -- Encode_Entry --
   ------------------

   procedure Encode_Entry
     (Item   : Partition_Entry;
      Bytes  : in out Storage_Array;
      Offset : Storage_Offset)
   is
   begin
      Put_64 (Bytes, Offset, Item.Partition_Type_Low);
      Put_64 (Bytes, Offset + 8, Item.Partition_Type_High);
      Put_64 (Bytes, Offset + 16, Item.Partition_Id_Low);
      Put_64 (Bytes, Offset + 24, Item.Partition_Id_High);
      Put_64 (Bytes, Offset + 32, Item.First_LBA);
      Put_64 (Bytes, Offset + 40, Item.Last_LBA);
      Put_64 (Bytes, Offset + 48, Item.Flags);

      for I in Item.Name'Range loop
         Bytes (Offset + 56 + Storage_Offset (I - 1)) :=
           Storage_Element (Character'Pos (Item.Name (I)));
      end loop;
   end Encode_Entry;

   -------------------
   -- Encode_Header --
   -------------------

   procedure Encode_Header
     (Item   : Header;
      Bytes  : in out Storage_Array)
   is
   begin
      Put_64 (Bytes, 0, Item.Magic);
      Put_32 (Bytes, 8, Item.Revision);
      Put_32 (Bytes, 12, Item.Header_Size);
      Put_32 (Bytes, 16, Item.Header_CRC);
      Put_32 (Bytes, 20, Item.Reserved_Zero);
      Put_64 (Bytes, 24, Item.Current_LBA);
      Put_64 (Bytes, 32, Item.Backup_LBA);
      Put_64 (Bytes, 40, Item.First_Usable_LBA);
      Put_64 (Bytes, 48, Item.Last_Usable_LBA);
      Put_64 (Bytes, 56, Item.Disk_GUID_Lo);
      Put_64 (Bytes, 64, Item.Disk_GUID_Hi);
      Put_64 (Bytes, 72, Item.Start_Partition_LBA);
      Put_32 (Bytes, 80, Item.Partition_Entry_Count);
      Put_32 (Bytes, 84, Item.Partition_Entry_Size);
      Put_32 (Bytes, 88, Item.Partition_Array_CRC);
   end Encode_Header;

   ------------
   -- Get_32 --
   ------------

   function Get_32
     (Bytes  : Storage_Array;
      Offset : Storage_Offset)
      return Word_32
   is
   begin
      return Word_32 (Bytes (Offset))
        + Word_32 (Bytes (Offset + 1)) * 2 ** 8
        + Word_32 (Bytes (Offset + 2)) * 2 ** 16
        + Word_32 (Bytes (Offset + 3)) * 2 ** 24;
   end Get_32;

   ------------
   -- Get_64 --
   ------------

   function Get_64
     (Bytes  : Storage_Array;
      Offset : Storage_Offset)
      return Word_64
   is
   begin
      return Word_64 (Get_32 (Bytes, Offset))
        + Word_64 (Get_32 (Bytes, Offset + 4)) * 2 ** 32;
   end Get_64;

   ----------------
   -- Header_CRC --
   ----------------

   function Header_CRC (Item : Header) return Word_32 is
      Copy  : Header := Item;
      Bytes : Storage_Array (0 .. Header_Bytes - 1) := (others => 0);
      CRC   : Word_32 := 16#FFFF_FFFF#;
   begin
      Copy.Header_CRC := 0;
      Encode_Header (Copy, Bytes);
      Update_CRC (CRC, Bytes);
      return not CRC;
   end Header_CRC;

   ------------------
   -- Header_Valid --
   ------------------

   function Header_Valid
     (Item        : Header;
      Header_LBA  : Word_64;
      Block_Size  : Natural;
      Block_Count : Word_64)
      return Boolean
   is
   begin
      if Item.Magic /= GPT_Magic
        or else Item.Header_Size /= Header_Bytes
        or else Item.Header_CRC /= Header_CRC (Item)
        or else Item.Current_LBA /= Header_LBA
        or else Item.Partition_Entry_Size /= Entry_Size
        or else Item.Partition_Entry_Count not in 1 .. Array_Entries
        or else Item.Start_Partition_LBA < 2
        or else Item.Start_Partition_LBA > Block_Count
        or else Item.First_Usable_LBA < 2
        or else Item.First_Usable_LBA > Item.Last_Usable_LBA
        or else Item.Last_Usable_LBA >= Block_Count
      then
         return False;
      end if;

      --  The entries are read a chunk at a time, so that is how many blocks
      --  there have to be from where they start.

      declare
         Chunk_Blocks : constant Natural := Chunk_Bytes / Block_Size;
         Chunks       : constant Natural :=
                          (Natural (Item.Partition_Entry_Count)
                           + Chunk_Entries - 1) / Chunk_Entries;
      begin
         return Block_Count - Item.Start_Partition_LBA
           >= Word_64 (Chunks) * Word_64 (Chunk_Blocks);
      end;
   end Header_Valid;

   ------------
   -- Put_32 --
   ------------

   procedure Put_32
     (Bytes  : in out Storage_Array;
      Offset : Storage_Offset;
      Value  : Word_32)
   is
      Rest : Word_32 := Value;
   begin
      for I in 0 .. 3 loop
         Bytes (Offset + Storage_Offset (I)) := Storage_Element (Rest mod 256);
         Rest := Rest / 256;
      end loop;
   end Put_32;

   ------------
   -- Put_64 --
   ------------

   procedure Put_64
     (Bytes  : in out Storage_Array;
      Offset : Storage_Offset;
      Value  : Word_64)
   is
   begin
      Put_32 (Bytes, Offset, Word_32 (Value mod 2 ** 32));
      Put_32 (Bytes, Offset + 4, Word_32 (Value / 2 ** 32));
   end Put_64;

   ----------------
   -- Update_CRC --
   ----------------

   procedure Update_CRC
     (CRC  : in out Word_32;
      Data : Storage_Array)
   is
   begin
      for Element of Data loop
         CRC := CRC xor Word_32 (Element);
         for Bit in 1 .. 8 loop
            if CRC mod 2 = 1 then
               CRC := (CRC / 2) xor 16#EDB8_8320#;
            else
               CRC := CRC / 2;
            end if;
         end loop;
      end loop;
   end Update_CRC;

end Rose.Devices.GPT_Format;
