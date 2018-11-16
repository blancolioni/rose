with System.Storage_Elements;

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

   function Partition_Entries_Per_Block
     (Block_Size : Rose.Devices.Block.Block_Size_Type)
      return Rose.Words.Word_32;

   procedure Read_Header
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type;
      Header       : out GPT_Header);

   procedure Write_Header
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type;
      Header       : GPT_Header);

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
      use Rose.Devices.Block;
      Block_Size        : constant Block_Size_Type :=
                            Rose.Devices.Block.Client.Get_Block_Size
                              (Block_Device);
      Entry_Count       : constant Word_32 :=
                            Partition_Entries_Per_Block (Block_Size);
      Header            : GPT_Header;
      Partition_Entries : GPT_Partition_Entry_Array (0 .. Entry_Count - 1);
   begin

      if not Has_GPT (Block_Device) then
         Rose.Console_IO.Put_Line ("add_partition: no gpt header");
         return;
      end if;

      Read_Header (Block_Device, Header);
      Rose.Devices.Block.Client.Read_Block
        (Block_Device, 2, Partition_Entries'Address);

--        Rose.Console_IO.Put ("partition ");
--        Rose.Console_IO.Put (Word_8 (Header.Partition_Entry_Count));
--        Rose.Console_IO.Put (": first ");
--        Rose.Console_IO.Put (Natural (First_Block));
--        Rose.Console_IO.Put ("; last ");
--        Rose.Console_IO.Put (Natural (Last_Block));
--        Rose.Console_IO.New_Line;

      Partition_Entries (Header.Partition_Entry_Count) :=
        GPT_Partition_Entry'
          (Partition_Type_Low  => Partition_Type_Low,
           Partition_Type_High => Partition_Type_High,
           Partition_Id_Low    => Word_64 (Header.Partition_Entry_Count),
           Partition_Id_High   => 0,
           First_LBA           => First_Block,
           Last_LBA            => Last_Block,
           Flags               => Partition_Flags,
           Name                => <>);
      Partition_Entries (Header.Partition_Entry_Count).Name
        (1 .. Partition_Name'Length)
        := Partition_Name;
      Header.Partition_Entry_Count := Header.Partition_Entry_Count + 1;

      Rose.Devices.Block.Client.Write_Block
        (Block_Device, 2, Partition_Entries'Address);
      Write_Header (Block_Device, Header);
      Rose.Console_IO.Put_Line ("partition added");
   end Add_Partition;

   -------------
   -- Has_GPT --
   -------------

   function Has_GPT
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type)
      return Boolean
   is
      use type Rose.Words.Word_64;
      Header  : GPT_Header;
   begin
      Read_Header (Block_Device, Header);
      return Header.Magic = GPT_Magic;
   end Has_GPT;

   --------------------
   -- Initialize_GPT --
   --------------------

   procedure Initialize_GPT
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type)
   is
      Header : constant GPT_Header := (others => <>);
   begin
      Rose.Console_IO.Put_Line ("writing default GPT to disk");
      Write_Header (Block_Device, Header);
   end Initialize_GPT;

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

   ---------------------------
   -- Write_Partition_Table --
   ---------------------------

   procedure Write_Partition_Table
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type)
   is
      use Rose.Words;
      Header : GPT_Header;
   begin
      if not Has_GPT (Block_Device) then
         Rose.Console_IO.Put_Line ("No GPT block found");
         return;
      end if;

      Read_Header (Block_Device, Header);

      if Header.Partition_Entry_Count = 0 then
         Rose.Console_IO.Put_Line ("No partitions in device");
         return;
      end if;

      declare
         use Rose.Devices.Block;
         Block_Size        : constant Block_Size_Type :=
                               Client.Get_Block_Size
                                 (Block_Device);
         Entry_Count       : constant Rose.Words.Word_32 :=
                               Partition_Entries_Per_Block (Block_Size);
         Partition_Entries : GPT_Partition_Entry_Array (1 .. Entry_Count);
      begin
         Client.Read_Block
           (Block_Device, 2, Partition_Entries'Address);
         Rose.Console_IO.Put_Line
           ("# Name            Start    End  Type      Flags");
         for I in 1 .. Header.Partition_Entry_Count loop
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

               Rose.Console_IO.Put (Natural (Part.First_LBA), 6);
               Rose.Console_IO.Put (Natural (Part.Last_LBA), 7);
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
      end;

   end Write_Partition_Table;

end Rose.Devices.GPT;
