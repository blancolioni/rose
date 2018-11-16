with Rose.Devices.Block;
with Rose.Words;

package ATA.Drives is

   type ATA_Drive_Index is range 0 .. 3;

   type ATA_Drive is private;

   function Get (Index : ATA_Drive_Index) return ATA_Drive;

   function Is_Initialized (Drive : ATA_Drive) return Boolean;
   function Is_Listening (Drive : ATA_Drive) return Boolean;
   function Is_Dead (Drive : ATA_Drive) return Boolean;

   function Block_Size
     (Drive : ATA_Drive)
      return Rose.Devices.Block.Block_Size_Type;

   function Block_Count
     (Drive : ATA_Drive)
      return Rose.Devices.Block.Block_Address_Type;

   procedure Initialize_Drive
     (Index        : ATA_Drive_Index;
      Command_Cap  : Rose.Capabilities.Capability;
      Control_Cap  : Rose.Capabilities.Capability;
      Data_Cap_8   : Rose.Capabilities.Capability;
      Data_Cap_16  : Rose.Capabilities.Capability;
      Base_DMA     : Rose.Words.Word_32;
      Is_Native    : Boolean);

private

   type ATA_Drive_Record is
      record
         Initialized  : Boolean := False;
         Listening    : Boolean := False;
         Dead         : Boolean := False;
         Native       : Boolean := False;
         Command_Cap  : Rose.Capabilities.Capability;
         Control_Cap  : Rose.Capabilities.Capability;
         Base_DMA     : Rose.Words.Word_32;
         Block_Size   : Rose.Devices.Block.Block_Size_Type;
         Block_Count  : Rose.Devices.Block.Block_Address_Type;
      end record;

   type ATA_Drive is access all ATA_Drive_Record;

   function Block_Size
     (Drive : ATA_Drive)
      return Rose.Devices.Block.Block_Size_Type
   is (Drive.Block_Size);

   function Block_Count
     (Drive : ATA_Drive)
      return Rose.Devices.Block.Block_Address_Type
   is (Drive.Block_Count);

end ATA.Drives;
