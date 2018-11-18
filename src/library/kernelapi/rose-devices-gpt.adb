with System.Storage_Elements;

with Rose.Devices.Block.Client.Table;
with Rose.Devices.Partitions;

with Rose.Console_IO;

package body Rose.Devices.GPT is

   Buffer : System.Storage_Elements.Storage_Array (1 .. 4096);

   GPT_Magic : constant Rose.Words.Word_64 := 16#5452_4150_2049_4645#;

   type GPT_Header is
      record
         Magic                 : Rose.Words.Word_64 := GPT_Magic;
         Revision              : Rose.Words.Word_32 := 16#0001_0000#;
         Header_Size           : Rose.Words.Word_32 := 16#0000_005C#;
         Header_CRC            : Rose.Words.Word_32 := 0;
         Reserved_Zero         : Rose.Words.Word_32 := 0;
         Current_LBA           : Rose.Devices.Block.Block_Address_Type := 0;
         Backup_LBA            : Rose.Devices.Block.Block_Address_Type := 0;
         First_Usable_LBA      : Rose.Devices.Block.Block_Address_Type := 0;
         Last_Usable_LBA       : Rose.Devices.Block.Block_Address_Type := 0;
         Disk_GUID_Lo          : Rose.Words.Word_64 := 0;
         Disk_GUID_Hi          : Rose.Words.Word_64 := 0;
         Start_Partition_LBA   : Rose.Devices.Block.Block_Address_Type := 0;
         Partition_Entry_Count : Rose.Words.Word_32 := 0;
         Partition_Entry_Size  : Rose.Words.Word_32 := 16#0000_0080#;
         Partition_Array_CRC   : Rose.Words.Word_32 := 0;
      end record
   with Size => 92 * 8;

   type GPT_Partition_Entry is
      record
         Partition_Type_Low  : Rose.Words.Word_64 := 0;
         Partition_Type_High : Rose.Words.Word_64 := 0;
         Partition_Id_Low    : Rose.Words.Word_64 := 0;
         Partition_Id_High   : Rose.Words.Word_64 := 0;
         First_LBA           : Rose.Devices.Block.Block_Address_Type := 0;
         Last_LBA            : Rose.Devices.Block.Block_Address_Type := 0;
         Flags               : Rose.Words.Word_64 := 0;
         Name                : String (1 .. 72) :=
                                 (others => Character'Val (0));
      end record
     with Size => 128 * 8;

   type GPT_Partition_Entry_Array is
     array (Rose.Words.Word_32 range <>) of GPT_Partition_Entry;

   type GPT_Record is
      record
         Header     : GPT_Header;
         Parts      : GPT_Partition_Entry_Array (0 .. 15);
         Block_Size : Rose.Devices.Block.Block_Size_Type;
         Dirty      : Boolean := False;
      end record;

   package Cached_Table is
     new Rose.Devices.Block.Client.Table (8, GPT_Record);

   GPT_Data          : GPT_Record;

   procedure Check_Cached
     (Device : Rose.Devices.Block.Client.Block_Device_Type);

   procedure Save_Changes
     (Device : Rose.Devices.Block.Client.Block_Device_Type);

   function Partition_Entries_Per_Block
     (Block_Size : Rose.Devices.Block.Block_Size_Type)
      return Rose.Words.Word_32;

   procedure Read_Header
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type;
      Header       : out GPT_Header);

   procedure Write_Header
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type;
      Header       : GPT_Header);

   procedure Write_Partition_Entries
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type;
      Entries      : GPT_Partition_Entry_Array);

   -------------------
   -- Add_Partition --
   -------------------

   procedure Add_Partition
     (Block_Device        : Rose.Devices.Block.Client.Block_Device_Type;
      First_Block         : Rose.Devices.Block.Block_Address_Type;
      Last_Block          : Rose.Devices.Block.Block_Address_Type;
      Partition_Type_Low  : Rose.Words.Word_64;
      Partition_Type_High : Rose.Words.Word_64;
      Partition_Flags     : Rose.Words.Word_64;
      Partition_Name      : String)
   is
      use Rose.Words;
      Header            : GPT_Header renames GPT_Data.Header;
      Part              : GPT_Partition_Entry renames
                            GPT_Data.Parts (Header.Partition_Entry_Count);
   begin

      Check_Cached (Block_Device);

      if not Has_GPT (Block_Device) then
         Rose.Console_IO.Put_Line ("add_partition: no gpt header");
         return;
      end if;

      Part :=
        GPT_Partition_Entry'
          (Partition_Type_Low  => Partition_Type_Low,
           Partition_Type_High => Partition_Type_High,
           Partition_Id_Low    => Word_64 (Header.Partition_Entry_Count),
           Partition_Id_High   => 0,
           First_LBA           => First_Block,
           Last_LBA            => Last_Block,
           Flags               => Partition_Flags,
           Name                => <>);

      declare
         Index : Natural := 0;
      begin
         for Ch of Partition_Name loop
            Index := Index + 1;
            Part.Name (Index) := Ch;
         end loop;
      end;

      Header.Partition_Entry_Count := Header.Partition_Entry_Count + 1;
      Save_Changes (Block_Device);
   end Add_Partition;

   ------------------
   -- Check_Cached --
   ------------------

   procedure Check_Cached
     (Device : Rose.Devices.Block.Client.Block_Device_Type)
   is
   begin
      if Cached_Table.Contains (Device) then
         Cached_Table.Get_Element (Device, GPT_Data);
         return;
      end if;

      declare
         use Rose.Words;
         use Rose.Devices.Block;
         Block_Size        : constant Block_Size_Type :=
                               Rose.Devices.Block.Client.Get_Block_Size
                                 (Device);
         Entry_Count       : constant Word_32 :=
                               Partition_Entries_Per_Block (Block_Size);
         Partition_Entries : GPT_Partition_Entry_Array (0 .. Entry_Count - 1);
      begin
         GPT_Data.Block_Size := Block_Size;
         Read_Header (Device, GPT_Data.Header);
         if GPT_Data.Header.Magic = GPT_Magic then
            if GPT_Data.Header.Partition_Entry_Count > 0 then
               Client.Read_Block
                 (Device, 2, Partition_Entries'Address);
               for I in 0 .. GPT_Data.Header.Partition_Entry_Count - 1 loop
                  GPT_Data.Parts (I) := Partition_Entries (I);
               end loop;
            end if;
         end if;
         Cached_Table.Insert (Device, GPT_Data);
      end;
   end Check_Cached;

   -----------
   -- Flush --
   -----------

   procedure Flush (Device : Rose.Devices.Block.Client.Block_Device_Type) is
      use type Rose.Words.Word_32;
   begin
      Check_Cached (Device);
      if GPT_Data.Dirty then
         Write_Header (Device, GPT_Data.Header);
         Write_Partition_Entries
           (Device,
            GPT_Data.Parts (0 .. GPT_Data.Header.Partition_Entry_Count - 1));
         GPT_Data.Dirty := False;
         Save_Changes (Device);
      end if;
   end Flush;

   -------------
   -- Has_GPT --
   -------------

   function Has_GPT
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type)
      return Boolean
   is
      use type Rose.Words.Word_64;
   begin
      Check_Cached (Block_Device);
      return GPT_Data.Header.Magic = GPT_Magic;
   end Has_GPT;

   --------------------
   -- Initialize_GPT --
   --------------------

   procedure Initialize_GPT
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type)
   is
      Header : constant GPT_Header := (others => <>);
   begin
      Check_Cached (Block_Device);
      GPT_Data.Header := Header;
      GPT_Data.Dirty := True;
      Save_Changes (Block_Device);
   end Initialize_GPT;

   ---------------------
   -- Partition_Count --
   ---------------------

   function Partition_Count
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type)
      return Natural
   is
   begin
      Check_Cached (Block_Device);
      return Natural (GPT_Data.Header.Partition_Entry_Count);
   end Partition_Count;

   ---------------------------------
   -- Partition_Entries_Per_Block --
   ---------------------------------

   function Partition_Entries_Per_Block
     (Block_Size : Rose.Devices.Block.Block_Size_Type)
      return Rose.Words.Word_32
   is
      use Rose.Words;
   begin
      return Word_32 (Block_Size) / 128;
   end Partition_Entries_Per_Block;

   -----------------
   -- Read_Header --
   -----------------

   procedure Read_Header
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type;
      Header       : out GPT_Header)
   is
      Block_Size : constant Rose.Devices.Block.Block_Size_Type :=
                     Rose.Devices.Block.Client.Get_Block_Size
                       (Block_Device);
   begin
      Buffer := (others => 0);
      Rose.Devices.Block.Client.Read_Block
        (Block_Device, 1, Buffer'Address);
      declare
         use System.Storage_Elements;
         Header_Storage : Storage_Array (1 .. Storage_Count (Block_Size));
         Header_Record  : GPT_Header;
         pragma Import (Ada, Header_Record);
         for Header_Record'Address use Header_Storage'Address;
      begin
         Header_Storage := Buffer (Header_Storage'Range);
         Header := Header_Record;
      end;
   end Read_Header;

   ----------------------------
   -- Report_Partition_Table --
   ----------------------------

   procedure Report_Partition_Table
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type)
   is
      use Rose.Words;
      Header : GPT_Header renames GPT_Data.Header;
      Partition_Entries : GPT_Partition_Entry_Array renames
                            GPT_Data.Parts;
   begin

      Check_Cached (Block_Device);

      if not Has_GPT (Block_Device) then
         Rose.Console_IO.Put_Line ("No GPT block found");
         return;
      end if;

      if Header.Partition_Entry_Count = 0 then
         Rose.Console_IO.Put_Line ("No partitions in device");
         return;
      end if;

      Rose.Console_IO.Put_Line
        ("# Name            Start        End      Type      Flags");
      for I in 0 .. Header.Partition_Entry_Count - 1 loop
         Rose.Console_IO.Put (Natural (I));
         Rose.Console_IO.Put (" ");
         declare
            Part : GPT_Partition_Entry renames Partition_Entries (I);
         begin
            for J in 1 .. 15 loop
               declare
                  Ch : constant Character := Part.Name (J);
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
     (Device : Rose.Devices.Block.Client.Block_Device_Type)
   is
   begin
      Cached_Table.Update (Device, GPT_Data);
   end Save_Changes;

   ------------------
   -- Write_Header --
   ------------------

   procedure Write_Header
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type;
      Header       : GPT_Header)
   is
      use Rose.Devices.Block;
      Block_Size : constant Block_Size_Type :=
                     Client.Get_Block_Size (Block_Device);
      Block_Count : constant Block_Address_Type :=
                     Client.Get_Block_Count (Block_Device);
   begin
      declare
         use System.Storage_Elements;
         Header_Storage : Storage_Array (1 .. Storage_Count (Block_Size));
         Header_Record  : GPT_Header;
         pragma Import (Ada, Header_Record);
         for Header_Record'Address use Header_Storage'Address;
      begin
         Buffer := (others => 0);
         Header_Record := Header;
         Buffer (Header_Storage'Range) := Header_Storage;
         Client.Write_Block (Block_Device, 1, Buffer'Address);
         Client.Write_Block
           (Block_Device, Block_Count - 1, Buffer'Address);
      end;
   end Write_Header;

   -----------------------------
   -- Write_Partition_Entries --
   -----------------------------

   procedure Write_Partition_Entries
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type;
      Entries      : GPT_Partition_Entry_Array)
   is
      use Rose.Words;
      use Rose.Devices.Block;
      Block_Size        : constant Block_Size_Type :=
                            Rose.Devices.Block.Client.Get_Block_Size
                              (Block_Device);
      Entry_Count       : constant Word_32 :=
                            Partition_Entries_Per_Block (Block_Size);
      Partition_Entries : GPT_Partition_Entry_Array (0 .. Entry_Count - 1);
   begin
      Partition_Entries (Entries'Range) := Entries;
      Rose.Devices.Block.Client.Write_Block
        (Block_Device, 2, Partition_Entries'Address);
   end Write_Partition_Entries;

end Rose.Devices.GPT;
