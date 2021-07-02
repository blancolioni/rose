with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Partitions.Client is

   type Partitions_Client is private;

   procedure Open_Cap_Set
     (Client          :    out Partitions_Client;
      Partition_Count : in     Rose.Capabilities.Capability;
      Get_Partition   : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Partitions_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Partition_Count (Item : Partitions_Client) return Natural;

   procedure Get_Partition
     (Item                : in     Partitions_Client;
      Index               : in     Positive;
      Partition_Type_Low  :    out Rose.Words.Word_64;
      Partition_Type_High :    out Rose.Words.Word_64;
      Partition_Flags     :    out Rose.Words.Word_64;
      Start_Address       :    out Rose.Words.Word_64;
      Length              :    out Rose.Words.Word_64);

   function Get_Interface_Cap (Item : Partitions_Client)
      return Rose.Capabilities.Capability;

   function Get_Partition_Count_Cap (Item : Partitions_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Partition_Cap (Item : Partitions_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Partitions_Client is
      record
         Is_Open         : Boolean := False;
         Interface_Cap   : Rose.Capabilities.Capability := 0;
         Partition_Count : Rose.Capabilities.Capability := 0;
         Get_Partition   : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Partitions.Client;
