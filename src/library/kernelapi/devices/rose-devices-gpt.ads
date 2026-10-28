--  A GUID partition table, as the UEFI specification describes it: a
--  protective MBR in block 0, a header in block 1 and 128 partition
--  entries after it, and a backup of the entries and the header at the
--  end of the disk, each checked by a CRC (#405).

with Rose.Interfaces.Block_Device.Client;
with Rose.Words;

package Rose.Devices.GPT is

   type GPT_State is (No_GPT, Valid_GPT, Invalid_GPT);
   --  Invalid_GPT is a disk with a GPT header that fails its checks, in
   --  both copies.  It is left alone, since it may hold someone's data.

   function State
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return GPT_State;

   function Has_GPT
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return Boolean
   is (State (Block_Device) = Valid_GPT);

   procedure Initialize_GPT
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);
   --  Start an empty GPT, with a new disk GUID.  Nothing is written until
   --  Flush.

   function Last_Usable_Block
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return Rose.Interfaces.Block_Device.Block_Address_Type;
   --  The last block a partition can use: the one before the backup
   --  entries.

   function Partition_Count
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   return Natural;
   --  The number of partitions in use.

   procedure Get_Partition_Details
     (Block_Device        :
      Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Partition_Index     : Positive;
      First_Block         : out
        Rose.Interfaces.Block_Device.Block_Address_Type;
      Last_Block          : out
        Rose.Interfaces.Block_Device.Block_Address_Type;
      Partition_Type_Low  : out Rose.Words.Word_64;
      Partition_Type_High : out Rose.Words.Word_64;
      Partition_Flags     : out Rose.Words.Word_64;
      Partition_Name      : out String;
      Partition_Name_Last : out Natural);
   --  Last_Block is the partition's last block, as in the GPT.

   procedure Add_Partition
     (Block_Device        :
      Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      First_Block         : Rose.Interfaces.Block_Device.Block_Address_Type;
      Last_Block          : Rose.Interfaces.Block_Device.Block_Address_Type;
      Partition_Type_Low  : Rose.Words.Word_64;
      Partition_Type_High : Rose.Words.Word_64;
      Partition_Flags     : Rose.Words.Word_64;
      Partition_Name      : String);
   --  Add a partition, with a new partition GUID.  Nothing is written
   --  until Flush.

   procedure Report_Partition_Table
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   procedure Flush
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);
   --  Write the protective MBR, both copies of the entries and the
   --  header, if anything has changed.

end Rose.Devices.GPT;
