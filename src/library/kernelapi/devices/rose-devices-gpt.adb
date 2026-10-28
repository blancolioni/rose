with System.Machine_Code;
with System.Storage_Elements;

with Rose.Interfaces.Block_Device.Client.Table;
with Rose.Devices.Partitions;

with Rose.Console_IO;

package body Rose.Devices.GPT is

   use Rose.Interfaces.Block_Device;
   use System.Storage_Elements;
   use Rose.Words;

   GPT_Magic : constant Word_64 := 16#5452_4150_2049_4645#;

   Entry_Size    : constant := 128;
   Array_Entries : constant := 128;
   Array_Bytes   : constant := Entry_Size * Array_Entries;
   --  The standard partition array: 128 entries of 128 bytes, 16 KiB

   Max_Partitions : constant := 16;
   --  The partitions in use that are kept in memory

   Chunk_Bytes   : constant := 4096;
   Chunk_Entries : constant := Chunk_Bytes / Entry_Size;
   --  The entries are read and written a page at a time, which is as
   --  much as one call carries.

   Name_Length : constant := 36;
   --  A partition name is 36 UTF-16LE code units.

   type GPT_Header is
      record
         Magic                 : Word_64 := GPT_Magic;
         Revision              : Word_32 := 16#0001_0000#;
         Header_Size           : Word_32 := 16#0000_005C#;
         Header_CRC            : Word_32 := 0;
         Reserved_Zero         : Word_32 := 0;
         Current_LBA           : Block_Address_Type := 1;
         Backup_LBA            : Block_Address_Type := 0;
         First_Usable_LBA      : Block_Address_Type := 0;
         Last_Usable_LBA       : Block_Address_Type := 0;
         Disk_GUID_Lo          : Word_64 := 0;
         Disk_GUID_Hi          : Word_64 := 0;
         Start_Partition_LBA   : Block_Address_Type := 2;
         Partition_Entry_Count : Word_32 := Array_Entries;
         Partition_Entry_Size  : Word_32 := Entry_Size;
         Partition_Array_CRC   : Word_32 := 0;
      end record
   with Size => 92 * 8;

   Header_Bytes : constant := 92;

   type GPT_Partition_Entry is
      record
         Partition_Type_Low  : Word_64 := 0;
         Partition_Type_High : Word_64 := 0;
         Partition_Id_Low    : Word_64 := 0;
         Partition_Id_High   : Word_64 := 0;
         First_LBA           : Block_Address_Type := 0;
         Last_LBA            : Block_Address_Type := 0;
         Flags               : Word_64 := 0;
         Name                : String (1 .. 2 * Name_Length) :=
                                 (others => Character'Val (0));
      end record
     with Size => Entry_Size * 8;

   type GPT_Partition_Entry_Array is
     array (Word_32 range <>) of GPT_Partition_Entry;

   type GPT_Record is
      record
         Header      : GPT_Header;
         Parts       : GPT_Partition_Entry_Array (0 .. Max_Partitions - 1);
         Count       : Word_32 := 0;
         State       : GPT_State := No_GPT;
         Block_Size  : Block_Size_Type := 512;
         Block_Count : Block_Address_Type := 0;
         Dirty       : Boolean := False;
      end record;

   package Cached_Table is
     new Rose.Interfaces.Block_Device.Client.Table (8, GPT_Record);

   GPT_Data          : GPT_Record;

   Random_State : Word_64 := 0;

   function Array_Blocks return Block_Address_Type
   is (Block_Address_Type (Array_Bytes / Natural (GPT_Data.Block_Size)));
   --  The blocks each copy of the partition array takes

   function Chunk_Blocks return Natural
   is (Chunk_Bytes / Natural (GPT_Data.Block_Size));

   function Entry_At (Index : Word_32) return GPT_Partition_Entry
   is (if Index < GPT_Data.Count
       then GPT_Data.Parts (Index)
       else (others => <>));
   --  Entry Index of the partition array: a partition in use, or an
   --  empty entry.

   procedure Check_Cached
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   procedure Update_CRC
     (CRC  : in out Word_32;
      Data : Storage_Array);
   --  CRC32, as GPT, zlib and Ethernet use it.  CRC starts as all ones,
   --  and the result is its complement.

   function Header_CRC (Header : GPT_Header) return Word_32;
   --  The CRC of the header, with its own CRC field taken as zero.

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
   --  right.

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
      Header : GPT_Header);
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

      if GPT_Data.Count = Max_Partitions then
         Rose.Console_IO.Put_Line ("add_partition: too many partitions");
         return;
      end if;

      declare
         Part : GPT_Partition_Entry renames GPT_Data.Parts (GPT_Data.Count);
      begin
         Part :=
           GPT_Partition_Entry'
             (Partition_Type_Low  => Partition_Type_Low,
              Partition_Type_High => Partition_Type_High,
              Partition_Id_Low    => 0,
              Partition_Id_High   => 0,
              First_LBA           => First_Block,
              Last_LBA            => Last_Block,
              Flags               => Partition_Flags,
              Name                => <>);

         New_GUID (Part.Partition_Id_Low, Part.Partition_Id_High);

         --  Names are UTF-16LE; Rose's are ASCII.

         for I in 1 .. Natural'Min (Partition_Name'Length, Name_Length) loop
            Part.Name (2 * I - 1) :=
              Partition_Name (Partition_Name'First + I - 1);
         end loop;
      end;

      GPT_Data.Count := GPT_Data.Count + 1;
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
            GPT_Data.Count := 0;
         else
            GPT_Data.State := No_GPT;
            GPT_Data.Count := 0;
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

      declare
         Part : GPT_Partition_Entry renames
                  GPT_Data.Parts (Word_32 (Partition_Index) - 1);
      begin
         First_Block := Part.First_LBA;
         Last_Block  := Part.Last_LBA;
         Partition_Type_Low := Part.Partition_Type_Low;
         Partition_Type_High := Part.Partition_Type_High;
         Partition_Flags := Part.Flags;
         Partition_Name_Last := 0;
         for I in 1 .. Natural'Min (Partition_Name'Length, Name_Length) loop
            exit when Character'Pos (Part.Name (2 * I - 1)) = 0;
            Partition_Name_Last := Partition_Name_Last + 1;
            Partition_Name (Partition_Name'First + I - 1) :=
              Part.Name (2 * I - 1);
         end loop;
      end;
   end Get_Partition_Details;

   ----------------
   -- Header_CRC --
   ----------------

   function Header_CRC (Header : GPT_Header) return Word_32 is
      Copy  : GPT_Header := Header;
      Bytes : Storage_Array (1 .. Header_Bytes);
      pragma Import (Ada, Bytes);
      for Bytes'Address use Copy'Address;
      CRC   : Word_32 := 16#FFFF_FFFF#;
   begin
      Copy.Header_CRC := 0;
      Update_CRC (CRC, Bytes);
      return not CRC;
   end Header_CRC;

   --------------------
   -- Initialize_GPT --
   --------------------

   procedure Initialize_GPT
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
   begin
      Check_Cached (Block_Device);
      GPT_Data.Header := (others => <>);
      New_GUID (GPT_Data.Header.Disk_GUID_Lo, GPT_Data.Header.Disk_GUID_Hi);
      GPT_Data.Count := 0;
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
      return GPT_Data.Block_Count - 2 - Array_Blocks;
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
      return Natural (GPT_Data.Count);
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
      Header : GPT_Header renames GPT_Data.Header;
   begin
      Valid := False;
      GPT_Data.Count := 0;

      declare
         Block  : Storage_Array (1 .. Storage_Count (GPT_Data.Block_Size));
         Stored : GPT_Header;
         pragma Import (Ada, Stored);
         for Stored'Address use Block'Address;
      begin
         Client.Read_Blocks (Device, Header_LBA, 1, Block);
         Header := Stored;
      end;

      Found_Magic := Header.Magic = GPT_Magic;

      if not Found_Magic
        or else Header.Header_Size /= Header_Bytes
        or else Header.Header_CRC /= Header_CRC (Header)
        or else Header.Current_LBA /= Header_LBA
        or else Header.Partition_Entry_Size /= Entry_Size
        or else Header.Partition_Entry_Count not in 1 .. Array_Entries
      then
         return;
      end if;

      declare
         Entries : GPT_Partition_Entry_Array (0 .. Chunk_Entries - 1);
         Chunk   : Storage_Array (1 .. Chunk_Bytes);
         pragma Import (Ada, Chunk);
         for Chunk'Address use Entries'Address;
         CRC     : Word_32 := 16#FFFF_FFFF#;
         Index   : Word_32 := 0;
         Start   : Block_Address_Type := Header.Start_Partition_LBA;
      begin
         while Index < Header.Partition_Entry_Count loop
            Client.Read_Blocks (Device, Start, Chunk_Blocks, Chunk);
            Start := Start + Block_Address_Type (Chunk_Blocks);

            for E of Entries loop
               exit when Index = Header.Partition_Entry_Count;

               declare
                  Bytes : Storage_Array (1 .. Entry_Size);
                  pragma Import (Ada, Bytes);
                  for Bytes'Address use E'Address;
               begin
                  Update_CRC (CRC, Bytes);
               end;

               if E.Partition_Type_Low /= 0
                 or else E.Partition_Type_High /= 0
               then
                  if GPT_Data.Count < Max_Partitions then
                     GPT_Data.Parts (GPT_Data.Count) := E;
                     GPT_Data.Count := GPT_Data.Count + 1;
                  else
                     Rose.Console_IO.Put_Line
                       ("gpt: too many partitions; ignoring the rest");
                  end if;
               end if;

               Index := Index + 1;
            end loop;
         end loop;

         Valid := (not CRC) = Header.Partition_Array_CRC;
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

      if GPT_Data.Count = 0 then
         Rose.Console_IO.Put_Line ("No partitions in device");
         return;
      end if;

      Rose.Console_IO.Put_Line
        ("# Name            Start        End      Type      Flags");
      for I in 0 .. GPT_Data.Count - 1 loop
         Rose.Console_IO.Put (Natural (I));
         Rose.Console_IO.Put (" ");
         declare
            Part : GPT_Partition_Entry renames GPT_Data.Parts (I);
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

            Rose.Console_IO.Put (Natural (Part.First_LBA), 10);
            Rose.Console_IO.Put (Natural (Part.Last_LBA), 11);
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

   ---------------
   -- Write_All --
   ---------------

   procedure Write_All
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      Last    : constant Block_Address_Type := GPT_Data.Block_Count - 1;
      Primary : GPT_Header := GPT_Data.Header;
      Backup  : GPT_Header;
      CRC     : Word_32 := 16#FFFF_FFFF#;
   begin
      for I in 0 .. Array_Entries - 1 loop
         declare
            E     : constant GPT_Partition_Entry := Entry_At (Word_32 (I));
            Bytes : Storage_Array (1 .. Entry_Size);
            pragma Import (Ada, Bytes);
            for Bytes'Address use E'Address;
         begin
            Update_CRC (CRC, Bytes);
         end;
      end loop;

      Primary.Magic := GPT_Magic;
      Primary.Revision := 16#0001_0000#;
      Primary.Header_Size := Header_Bytes;
      Primary.Reserved_Zero := 0;
      Primary.First_Usable_LBA := 2 + Array_Blocks;
      Primary.Last_Usable_LBA := Last - 1 - Array_Blocks;
      Primary.Partition_Entry_Count := Array_Entries;
      Primary.Partition_Entry_Size := Entry_Size;
      Primary.Partition_Array_CRC := not CRC;

      Backup := Primary;
      Backup.Current_LBA := Last;
      Backup.Backup_LBA := 1;
      Backup.Start_Partition_LBA := Last - Array_Blocks;
      Backup.Header_CRC := Header_CRC (Backup);

      Primary.Current_LBA := 1;
      Primary.Backup_LBA := Last;
      Primary.Start_Partition_LBA := 2;
      Primary.Header_CRC := Header_CRC (Primary);

      Write_Protective_MBR (Device);
      Write_Entries (Device, Primary.Start_Partition_LBA);
      Write_Entries (Device, Backup.Start_Partition_LBA);
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
      Entries : GPT_Partition_Entry_Array (0 .. Chunk_Entries - 1);
      Chunk   : Storage_Array (1 .. Chunk_Bytes);
      pragma Import (Ada, Chunk);
      for Chunk'Address use Entries'Address;
      Address : Block_Address_Type := Start;
   begin
      for First in 0 .. Array_Entries / Chunk_Entries - 1 loop
         for I in Entries'Range loop
            Entries (I) :=
              Entry_At (Word_32 (First * Chunk_Entries) + I);
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
      Header : GPT_Header)
   is
      Block  : Storage_Array (1 .. Storage_Count (GPT_Data.Block_Size)) :=
                 (others => 0);
      Stored : GPT_Header;
      pragma Import (Ada, Stored);
      for Stored'Address use Block'Address;
   begin
      Stored := Header;
      Client.Write_Blocks (Device, Header.Current_LBA, 1, Block);
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
