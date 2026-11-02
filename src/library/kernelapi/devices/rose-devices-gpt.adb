with System.Machine_Code;
with System.Storage_Elements;

with Rose.Interfaces.Block_Device.Client.Table;
with Rose.Devices.GPT_Format;
with Rose.Devices.Partitions;

with Rose.Console_IO;

package body Rose.Devices.GPT is

   package Format renames Rose.Devices.GPT_Format;

   use Rose.Interfaces.Block_Device;
   use System.Storage_Elements;
   use Rose.Words;

   --  The format of the table, and what makes it valid, is in
   --  Rose.Devices.GPT_Format, which is in SPARK.  This package reads and
   --  writes it, and keeps the table of each disk that has been asked
   --  about.

   Entry_Size    : constant := Format.Entry_Size;
   Array_Entries : constant := Format.Array_Entries;
   Max_Partitions : constant := Format.Max_Partitions;
   Chunk_Bytes   : constant := Format.Chunk_Bytes;
   Chunk_Entries : constant := Format.Chunk_Entries;
   Name_Length   : constant := Format.Name_Length;

   type GPT_Record is
      record
         Header      : Format.Header;
         Table       : Format.Partition_Table;
         State       : GPT_State := No_GPT;
         Block_Size  : Block_Size_Type := 512;
         Block_Count : Block_Address_Type := 0;
         Dirty       : Boolean := False;
      end record;

   package Cached_Table is
     new Rose.Interfaces.Block_Device.Client.Table (8, GPT_Record);

   GPT_Data          : GPT_Record;

   Random_State : Word_64 := 0;

   function Geometry_Valid return Boolean
   is (Format.Geometry_Valid
         (Natural (GPT_Data.Block_Size), Word_64 (GPT_Data.Block_Count)));
   --  The disk is one that a GPT can be on.  Until it is, nothing here
   --  does anything with the disk's blocks.

   function Array_Blocks return Block_Address_Type
   is (if Geometry_Valid
       then Block_Address_Type
              (Format.Array_Blocks (Natural (GPT_Data.Block_Size)))
       else 0);
   --  The blocks each copy of the partition array takes

   function Chunk_Blocks return Natural
   is (if Geometry_Valid
       then Chunk_Bytes / Natural (GPT_Data.Block_Size)
       else 0);

   function Entry_At (Index : Natural) return Format.Partition_Entry
   is (if Index < GPT_Data.Table.Count
       then GPT_Data.Table.Parts (Index)
       else (others => <>));
   --  Entry Index of the partition array: a partition in use, or an
   --  empty entry.

   function To_Natural (Value : Word_64) return Natural
   is (Natural (Word_64'Min (Value, Word_64 (Natural'Last))));

   procedure Check_Cached
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   procedure New_GUID (Low, High : out Word_64);
   --  A random (version 4) GUID.

   function Random_Word return Word_64;
   --  A pseudo-random word, seeded from the CPU's time stamp counter.

   procedure Read_Copy
     (Device      : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Header_LBA  : Block_Address_Type;
      Found_Magic : out Boolean;
      Valid       : out Boolean);
   --  Read the header at Header_LBA and the entries it points to into
   --  GPT_Data.  Valid if the signature, size, address and both CRCs are
   --  right, and so is what they say about the disk and the partitions
   --  (Format.Header_Valid and Format.Add).  The geometry of the disk
   --  must be valid.

   procedure Save_Changes
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   procedure Write_All
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);
   --  Write the protective MBR, both copies of the entries, and the
   --  backup header and then the primary one.

   procedure Write_Entries
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Start  : Block_Address_Type);

   procedure Write_Header
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Header : Format.Header);
   --  Write Header to its Current_LBA.

   procedure Write_Protective_MBR
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);
   --  Write the protective MBR that a GPT disk has in block 0: a single
   --  partition of type EE covering the disk, so that tools which only
   --  know MBRs leave it alone.  GRUB looks for it too (#322).  The
   --  boot code in the first 440 bytes is kept.

   -------------------
   -- Add_Partition --
   -------------------

   procedure Add_Partition
     (Block_Device        : Client.Block_Device_Client;
      First_Block         : Block_Address_Type;
      Last_Block          : Block_Address_Type;
      Partition_Type_Low  : Word_64;
      Partition_Type_High : Word_64;
      Partition_Flags     : Word_64;
      Partition_Name      : String)
   is
   begin

      Check_Cached (Block_Device);

      if not Has_GPT (Block_Device) then
         Rose.Console_IO.Put_Line ("add_partition: no gpt header");
         return;
      end if;

      if GPT_Data.Table.Count = Max_Partitions then
         Rose.Console_IO.Put_Line ("add_partition: too many partitions");
         return;
      end if;

      declare
         Item  : Format.Partition_Entry :=
           (Partition_Type_Low  => Partition_Type_Low,
            Partition_Type_High => Partition_Type_High,
            Partition_Id_Low    => 0,
            Partition_Id_High   => 0,
            First_LBA           => Word_64 (First_Block),
            Last_LBA            => Word_64 (Last_Block),
            Flags               => Partition_Flags,
            Name                => <>);
         Added : Boolean;
      begin
         New_GUID (Item.Partition_Id_Low, Item.Partition_Id_High);

         --  Names are UTF-16LE; Rose's are ASCII.

         for I in 1 .. Natural'Min (Partition_Name'Length, Name_Length) loop
            Item.Name (2 * I - 1) :=
              Partition_Name (Partition_Name'First + I - 1);
         end loop;

         Format.Add
           (GPT_Data.Table, Item,
            Format.First_Usable_Block (Natural (GPT_Data.Block_Size)),
            Format.Last_Usable_Block
              (Natural (GPT_Data.Block_Size), Word_64 (GPT_Data.Block_Count)),
            Added);

         if not Added then
            Rose.Console_IO.Put_Line
              ("add_partition: the partition is empty, or is not in the "
               & "blocks that partitions may use, or overlaps another");
            return;
         end if;
      end;

      GPT_Data.Dirty := True;
      Save_Changes (Block_Device);
   end Add_Partition;

   ------------------
   -- Check_Cached --
   ------------------

   procedure Check_Cached
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      Primary_Magic, Primary_Valid : Boolean;
      Backup_Magic, Backup_Valid   : Boolean;
   begin
      if Cached_Table.Contains (Device) then
         Cached_Table.Get_Element (Device, GPT_Data);
         return;
      end if;

      GPT_Data := (others => <>);
      Client.Get_Parameters
        (Device, GPT_Data.Block_Count, GPT_Data.Block_Size);

      if not Geometry_Valid then
         Rose.Console_IO.Put_Line
           ("gpt: this disk's block size or size is not one that a GPT "
            & "can be on; not using it");
         GPT_Data.State := Invalid_GPT;
         Cached_Table.Insert (Device, GPT_Data);
         return;
      end if;

      Read_Copy (Device, 1, Primary_Magic, Primary_Valid);

      if Primary_Valid then
         GPT_Data.State := Valid_GPT;
      else
         Read_Copy
           (Device, GPT_Data.Block_Count - 1, Backup_Magic, Backup_Valid);

         if Backup_Valid then
            Rose.Console_IO.Put_Line
              ("gpt: the primary GPT is damaged; restoring it from the "
               & "backup");
            GPT_Data.State := Valid_GPT;
            Write_All (Device);
         elsif Primary_Magic or else Backup_Magic then
            Rose.Console_IO.Put_Line
              ("gpt: this disk's GPT fails its checks; not using it");
            GPT_Data.State := Invalid_GPT;
            GPT_Data.Table.Count := 0;
         else
            GPT_Data.State := No_GPT;
            GPT_Data.Table.Count := 0;
         end if;
      end if;

      Cached_Table.Insert (Device, GPT_Data);
   end Check_Cached;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
   begin
      Check_Cached (Device);
      if GPT_Data.Dirty then
         Write_All (Device);
         Save_Changes (Device);
      end if;
   end Flush;

   ---------------------------
   -- Get_Partition_Details --
   ---------------------------

   procedure Get_Partition_Details
     (Block_Device        :
      Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Partition_Index     : Positive;
      First_Block         : out
        Rose.Interfaces.Block_Device.Block_Address_Type;
      Last_Block          : out
        Rose.Interfaces.Block_Device.Block_Address_Type;
      Partition_Type_Low  : out Word_64;
      Partition_Type_High : out Word_64;
      Partition_Flags     : out Word_64;
      Partition_Name      : out String;
      Partition_Name_Last : out Natural)
   is
   begin
      Check_Cached (Block_Device);

      First_Block := 0;
      Last_Block := 0;
      Partition_Type_Low := 0;
      Partition_Type_High := 0;
      Partition_Flags := 0;
      Partition_Name := (others => ' ');
      Partition_Name_Last := 0;

      --  A partition that is not there is empty.

      if Partition_Index > GPT_Data.Table.Count then
         return;
      end if;

      declare
         Part : Format.Partition_Entry renames
                  GPT_Data.Table.Parts (Partition_Index - 1);
      begin
         First_Block := Block_Address_Type (Part.First_LBA);
         Last_Block  := Block_Address_Type (Part.Last_LBA);
         Partition_Type_Low := Part.Partition_Type_Low;
         Partition_Type_High := Part.Partition_Type_High;
         Partition_Flags := Part.Flags;
         for I in 1 .. Natural'Min (Partition_Name'Length, Name_Length) loop
            exit when Character'Pos (Part.Name (2 * I - 1)) = 0;
            Partition_Name_Last := Partition_Name_Last + 1;
            Partition_Name (Partition_Name'First + I - 1) :=
              Part.Name (2 * I - 1);
         end loop;
      end;
   end Get_Partition_Details;

   --------------------
   -- Initialize_GPT --
   --------------------

   procedure Initialize_GPT
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
   begin
      Check_Cached (Block_Device);

      --  The disk is not one that a GPT can be on, or has a GPT that it
      --  would destroy.

      if GPT_Data.State = Invalid_GPT then
         Rose.Console_IO.Put_Line ("initialize_gpt: not using this disk");
         return;
      end if;

      GPT_Data.Header := (others => <>);
      New_GUID (GPT_Data.Header.Disk_GUID_Lo, GPT_Data.Header.Disk_GUID_Hi);
      GPT_Data.Table.Count := 0;
      GPT_Data.State := Valid_GPT;
      GPT_Data.Dirty := True;
      Save_Changes (Block_Device);
   end Initialize_GPT;

   -----------------------
   -- Last_Usable_Block --
   -----------------------

   function Last_Usable_Block
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return Rose.Interfaces.Block_Device.Block_Address_Type
   is
   begin
      Check_Cached (Block_Device);

      if not Geometry_Valid then
         return 0;
      end if;

      return Block_Address_Type
        (Format.Last_Usable_Block
           (Natural (GPT_Data.Block_Size), Word_64 (GPT_Data.Block_Count)));
   end Last_Usable_Block;

   --------------
   -- New_GUID --
   --------------

   procedure New_GUID (Low, High : out Word_64) is
   begin
      Low := Random_Word;
      High := Random_Word;

      --  The version, 4, is the top four bits of byte 7, and the variant
      --  is the top two bits of byte 8, which are 10.

      Low := (Low and 16#0FFF_FFFF_FFFF_FFFF#) or 16#4000_0000_0000_0000#;
      High := (High and 16#FFFF_FFFF_FFFF_FF3F#) or 16#80#;
   end New_GUID;

   ---------------------
   -- Partition_Count --
   ---------------------

   function Partition_Count
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return Natural
   is
   begin
      Check_Cached (Block_Device);
      return GPT_Data.Table.Count;
   end Partition_Count;

   -----------------
   -- Random_Word --
   -----------------

   function Random_Word return Word_64 is
      Low, High : Word_32;
      Z         : Word_64;
   begin
      System.Machine_Code.Asm
        ("rdtsc",
         Outputs  => (Word_32'Asm_Output ("=a", Low),
                      Word_32'Asm_Output ("=d", High)),
         Volatile => True);

      --  SplitMix64, stepped by the time stamp counter as well as its
      --  constant, so that each word depends on when it was asked for.

      Random_State :=
        Random_State + Word_64 (High) * 2 ** 32 + Word_64 (Low)
        + 16#9E37_79B9_7F4A_7C15#;
      Z := Random_State;
      Z := (Z xor (Z / 2 ** 30)) * 16#BF58_476D_1CE4_E5B9#;
      Z := (Z xor (Z / 2 ** 27)) * 16#94D0_49BB_1331_11EB#;
      return Z xor (Z / 2 ** 31);
   end Random_Word;

   ---------------
   -- Read_Copy --
   ---------------

   procedure Read_Copy
     (Device      : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Header_LBA  : Block_Address_Type;
      Found_Magic : out Boolean;
      Valid       : out Boolean)
   is
      Header : Format.Header renames GPT_Data.Header;
   begin
      Valid := False;
      GPT_Data.Table.Count := 0;

      declare
         Block : Storage_Array (0 .. Storage_Count (GPT_Data.Block_Size) - 1);
      begin
         Client.Read_Blocks (Device, Header_LBA, 1, Block);
         Header := Format.Decode_Header (Block);
      end;

      Found_Magic := Header.Magic = Format.GPT_Magic;

      if not Format.Header_Valid
               (Header, Word_64 (Header_LBA),
                Natural (GPT_Data.Block_Size), Word_64 (GPT_Data.Block_Count))
      then
         return;
      end if;

      declare
         Entry_Count : constant Natural :=
                         Natural (Header.Partition_Entry_Count);
         Chunk       : Storage_Array (0 .. Chunk_Bytes - 1);
         CRC         : Word_32 := 16#FFFF_FFFF#;
         Index       : Natural := 0;
         Start       : Block_Address_Type :=
                         Block_Address_Type (Header.Start_Partition_LBA);
         Intact      : Boolean := True;
      begin
         while Index < Entry_Count loop
            Client.Read_Blocks (Device, Start, Chunk_Blocks, Chunk);
            Start := Start + Block_Address_Type (Chunk_Blocks);

            for K in 0 .. Chunk_Entries - 1 loop
               exit when Index = Entry_Count;

               declare
                  Offset : constant Storage_Offset :=
                             Storage_Offset (K) * Entry_Size;
                  E      : constant Format.Partition_Entry :=
                             Format.Decode_Entry (Chunk, Offset);
                  Added  : Boolean;
               begin
                  Format.Update_CRC
                    (CRC, Chunk (Offset .. Offset + Entry_Size - 1));

                  if E.Partition_Type_Low /= 0
                    or else E.Partition_Type_High /= 0
                  then
                     if GPT_Data.Table.Count < Max_Partitions then
                        Format.Add
                          (GPT_Data.Table, E,
                           Header.First_Usable_LBA, Header.Last_Usable_LBA,
                           Added);

                        --  A partition that is empty, outside the blocks
                        --  that partitions may use, or overlapping
                        --  another, makes the table not valid.

                        if not Added then
                           Intact := False;
                        end if;
                     else
                        Rose.Console_IO.Put_Line
                          ("gpt: too many partitions; ignoring the rest");
                     end if;
                  end if;

                  Index := Index + 1;
               end;
            end loop;
         end loop;

         Valid := Intact and then (not CRC) = Header.Partition_Array_CRC;
      end;
   end Read_Copy;

   ----------------------------
   -- Report_Partition_Table --
   ----------------------------

   procedure Report_Partition_Table
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
   begin

      Check_Cached (Block_Device);

      if not Has_GPT (Block_Device) then
         Rose.Console_IO.Put_Line ("No GPT block found");
         return;
      end if;

      if GPT_Data.Table.Count = 0 then
         Rose.Console_IO.Put_Line ("No partitions in device");
         return;
      end if;

      Rose.Console_IO.Put_Line
        ("# Name            Start        End      Type      Flags");
      for I in 0 .. GPT_Data.Table.Count - 1 loop
         Rose.Console_IO.Put (I);
         Rose.Console_IO.Put (" ");
         declare
            Part : Format.Partition_Entry renames GPT_Data.Table.Parts (I);
         begin
            for J in 1 .. 15 loop
               declare
                  Ch : constant Character := Part.Name (2 * J - 1);
               begin
                  if Ch = Character'Val (0) then
                     Rose.Console_IO.Put (' ');
                  else
                     Rose.Console_IO.Put (Ch);
                  end if;
               end;
            end loop;

            Rose.Console_IO.Put (To_Natural (Part.First_LBA), 10);
            Rose.Console_IO.Put (To_Natural (Part.Last_LBA), 11);
            Rose.Console_IO.Put ("  ");

            declare
               use Rose.Devices.Partitions;
            begin
               if Part.Partition_Type_Low = Swap_Id_Low
                 and then Part.Partition_Type_High = Swap_Id_High
               then
                  Rose.Console_IO.Put
                    ("rose-swap ");
               elsif Part.Partition_Type_Low = Log_Id_Low
                 and then Part.Partition_Type_High = Log_Id_High
               then
                  Rose.Console_IO.Put
                    ("rose-log  ");
               elsif Part.Partition_Type_Low = BIOS_Boot_Id_Low
                 and then Part.Partition_Type_High = BIOS_Boot_Id_High
               then
                  Rose.Console_IO.Put
                    ("bios-boot ");
               elsif Part.Partition_Type_Low = EFI_System_Id_Low
                 and then Part.Partition_Type_High = EFI_System_Id_High
               then
                  Rose.Console_IO.Put
                    ("efi-system");
               else
                  Rose.Console_IO.Put
                    ("unknown   ");
               end if;
            end;

         end;
         Rose.Console_IO.New_Line;
      end loop;

   end Report_Partition_Table;

   ------------------
   -- Save_Changes --
   ------------------

   procedure Save_Changes
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
   begin
      Cached_Table.Update (Device, GPT_Data);
   end Save_Changes;

   -----------
   -- State --
   -----------

   function State
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return GPT_State
   is
   begin
      Check_Cached (Block_Device);
      return GPT_Data.State;
   end State;

   ---------------
   -- Write_All --
   ---------------

   procedure Write_All
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      Block_Size : constant Natural := Natural (GPT_Data.Block_Size);
      Block_Count : constant Word_64 := Word_64 (GPT_Data.Block_Count);
      Last    : constant Word_64 := Block_Count - 1;
      Primary : Format.Header := GPT_Data.Header;
      Backup  : Format.Header;
      CRC     : Word_32 := 16#FFFF_FFFF#;
      Bytes   : Storage_Array (0 .. Entry_Size - 1) := (others => 0);
   begin
      for I in 0 .. Array_Entries - 1 loop
         Format.Encode_Entry (Entry_At (I), Bytes, 0);
         Format.Update_CRC (CRC, Bytes);
      end loop;

      Primary.Magic := Format.GPT_Magic;
      Primary.Revision := 16#0001_0000#;
      Primary.Header_Size := Format.Header_Bytes;
      Primary.Reserved_Zero := 0;
      Primary.First_Usable_LBA := Format.First_Usable_Block (Block_Size);
      Primary.Last_Usable_LBA :=
        Format.Last_Usable_Block (Block_Size, Block_Count);
      Primary.Partition_Entry_Count := Array_Entries;
      Primary.Partition_Entry_Size := Entry_Size;
      Primary.Partition_Array_CRC := not CRC;

      Backup := Primary;
      Backup.Current_LBA := Last;
      Backup.Backup_LBA := 1;
      Backup.Start_Partition_LBA := Last - Word_64 (Array_Blocks);
      Backup.Header_CRC := Format.Header_CRC (Backup);

      Primary.Current_LBA := 1;
      Primary.Backup_LBA := Last;
      Primary.Start_Partition_LBA := 2;
      Primary.Header_CRC := Format.Header_CRC (Primary);

      Write_Protective_MBR (Device);
      Write_Entries (Device, Block_Address_Type (Primary.Start_Partition_LBA));
      Write_Entries (Device, Block_Address_Type (Backup.Start_Partition_LBA));
      Write_Header (Device, Backup);
      Write_Header (Device, Primary);

      GPT_Data.Header := Primary;
      GPT_Data.Dirty := False;
   end Write_All;

   -------------------
   -- Write_Entries --
   -------------------

   procedure Write_Entries
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Start  : Block_Address_Type)
   is
      Chunk   : Storage_Array (0 .. Chunk_Bytes - 1) := (others => 0);
      Address : Block_Address_Type := Start;
   begin
      for First in 0 .. Array_Entries / Chunk_Entries - 1 loop
         for K in 0 .. Chunk_Entries - 1 loop
            Format.Encode_Entry
              (Entry_At (First * Chunk_Entries + K),
               Chunk, Storage_Offset (K) * Entry_Size);
         end loop;
         Client.Write_Blocks (Device, Address, Chunk_Blocks, Chunk);
         Address := Address + Block_Address_Type (Chunk_Blocks);
      end loop;
   end Write_Entries;

   ------------------
   -- Write_Header --
   ------------------

   procedure Write_Header
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Header : Format.Header)
   is
      Block : Storage_Array (0 .. Storage_Count (GPT_Data.Block_Size) - 1) :=
                (others => 0);
   begin
      Format.Encode_Header (Header, Block);
      Client.Write_Blocks
        (Device, Block_Address_Type (Header.Current_LBA), 1, Block);
   end Write_Header;

   --------------------------
   -- Write_Protective_MBR --
   --------------------------

   procedure Write_Protective_MBR
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      Block_Size  : Block_Size_Type;
      Block_Count : Block_Address_Type;
   begin
      Client.Get_Parameters (Block_Device, Block_Count, Block_Size);

      declare
         MBR   : Storage_Array (0 .. Storage_Offset (Block_Size) - 1);
         Size  : constant Block_Address_Type :=
                   Block_Address_Type'Min (Block_Count - 1, 16#FFFF_FFFF#);
         Entry_Start : constant := 446;

         procedure Put_32
           (Offset : Storage_Offset;
            Value  : Word_32);

         ------------
         -- Put_32 --
         ------------

         procedure Put_32
           (Offset : Storage_Offset;
            Value  : Word_32)
         is
            X : Word_32 := Value;
         begin
            for I in 0 .. 3 loop
               MBR (Offset + Storage_Offset (I)) :=
                 Storage_Element (X mod 256);
               X := X / 256;
            end loop;
         end Put_32;

      begin
         Client.Read_Blocks (Block_Device, 0, 1, MBR);
         MBR (440 .. MBR'Last) := (others => 0);

         --  The partition entry: not bootable, CHS 0/0/2 to the largest
         --  CHS address, type EE, from block 1 to the end of the disk.

         MBR (Entry_Start + 1 .. Entry_Start + 3) := (16#00#, 16#02#, 16#00#);
         MBR (Entry_Start + 4) := 16#EE#;
         MBR (Entry_Start + 5 .. Entry_Start + 7) := (16#FF#, 16#FF#, 16#FF#);
         Put_32 (Entry_Start + 8, 1);
         Put_32 (Entry_Start + 12, Word_32 (Size));

         MBR (510) := 16#55#;
         MBR (511) := 16#AA#;

         Client.Write_Blocks (Block_Device, 0, 1, MBR);
      end;
   end Write_Protective_MBR;

end Rose.Devices.GPT;
