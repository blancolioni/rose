with Rose.Server;
with Rose.Words;

package Rose.Interfaces.Partitions.Server is

   type Partition_Count_Handler is access
     function (Id : Rose.Objects.Capability_Identifier) return Natural;

   type Get_Partition_Handler is access
     procedure
       (Id                  : in     Rose.Objects.Capability_Identifier;
        Index               : in     Positive;
        Partition_Type_Low  :    out Rose.Words.Word_64;
        Partition_Type_High :    out Rose.Words.Word_64;
        Partition_Flags     :    out Rose.Words.Word_64;
        Start_Address       :    out Rose.Words.Word_64;
        Length              :    out Rose.Words.Word_64);

   procedure Create_Server
     (Server_Context  : in out Rose.Server.Server_Context;
      Partition_Count : in     Partition_Count_Handler;
      Get_Partition   : in     Get_Partition_Handler;
      Instanced       : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context  : in out Rose.Server.Server_Context;
      Partition_Count : in     Partition_Count_Handler;
      Get_Partition   : in     Get_Partition_Handler;
      Instanced       : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context  : in out Rose.Server.Server_Context;
      Partition_Count : in     Partition_Count_Handler;
      Get_Partition   : in     Get_Partition_Handler);

private

end Rose.Interfaces.Partitions.Server;
