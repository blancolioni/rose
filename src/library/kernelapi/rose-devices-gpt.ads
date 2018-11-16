with Rose.Devices.Block.Client;
with Rose.Words;

package Rose.Devices.GPT is

   function Has_GPT
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type)
      return Boolean;

   procedure Initialize_GPT
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type);

   procedure Add_Partition
     (Block_Device        : Rose.Devices.Block.Client.Block_Device_Type;
      First_Block         : Rose.Devices.Block.Block_Address_Type;
      Last_Block          : Rose.Devices.Block.Block_Address_Type;
      Partition_Type_Low  : Rose.Words.Word_64;
      Partition_Type_High : Rose.Words.Word_64;
      Partition_Flags     : Rose.Words.Word_64;
      Partition_Name      : String);

   procedure Write_Partition_Table
     (Block_Device : Rose.Devices.Block.Client.Block_Device_Type);

end Rose.Devices.GPT;
