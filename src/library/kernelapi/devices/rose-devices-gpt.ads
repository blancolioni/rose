with Rose.Interfaces.Block_Device.Client;
with Rose.Words;

package Rose.Devices.GPT is

   function Has_GPT
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return Boolean;

   procedure Initialize_GPT
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   function Partition_Count
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   return Natural;

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

   procedure Add_Partition
     (Block_Device        :
      Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      First_Block         : Rose.Interfaces.Block_Device.Block_Address_Type;
      Last_Block          : Rose.Interfaces.Block_Device.Block_Address_Type;
      Partition_Type_Low  : Rose.Words.Word_64;
      Partition_Type_High : Rose.Words.Word_64;
      Partition_Flags     : Rose.Words.Word_64;
      Partition_Name      : String);

   procedure Report_Partition_Table
     (Block_Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   procedure Flush
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

end Rose.Devices.GPT;
