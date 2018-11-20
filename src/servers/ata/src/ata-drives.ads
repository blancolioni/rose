with System.Storage_Elements;
with Rose.Devices.Block;
with Rose.Words;

package ATA.Drives is

   type ATA_Drive_Index is range 0 .. 3;

   type ATA_Drive is private;

   function Get (Index : ATA_Drive_Index) return ATA_Drive;

   function Is_Initialized (Drive : ATA_Drive) return Boolean;
   function Is_Listening (Drive : ATA_Drive) return Boolean;
   function Is_Dead (Drive : ATA_Drive) return Boolean;

   procedure Initialize_Drive
     (Index             : ATA_Drive_Index;
      Command_Cap       : Rose.Capabilities.Capability;
      Control_Cap       : Rose.Capabilities.Capability;
      Data_Cap_8        : Rose.Capabilities.Capability;
      Data_Read_Cap_16  : Rose.Capabilities.Capability;
      Data_Write_Cap_16 : Rose.Capabilities.Capability;
      Base_DMA          : Rose.Words.Word_32;
      Is_Native         : Boolean);

   function Get_Parameters_Cap
     (Drive : ATA_Drive)
      return Rose.Capabilities.Capability;

   function Read_Block_Cap
     (Drive : ATA_Drive)
      return Rose.Capabilities.Capability;

   function Write_Block_Cap
     (Drive : ATA_Drive)
      return Rose.Capabilities.Capability;

   function Block_Size
     (Drive : ATA_Drive)
      return Rose.Devices.Block.Block_Size_Type;

   function Block_Count
     (Drive : ATA_Drive)
      return Rose.Devices.Block.Block_Address_Type;

   procedure Read_Block
     (Index   : ATA_Drive_Index;
      Address : Rose.Devices.Block.Block_Address_Type;
      Buffer  : out System.Storage_Elements.Storage_Array);

   procedure Write_Block
     (Index   : ATA_Drive_Index;
      Address : Rose.Devices.Block.Block_Address_Type;
      Buffer  : System.Storage_Elements.Storage_Array);

private

   type ATA_Drive_Record is
      record
         Initialized        : Boolean := False;
         Listening          : Boolean := False;
         Dead               : Boolean := False;
         Native             : Boolean := False;
         Atapi              : Boolean := False;
         Command_Cap        : Rose.Capabilities.Capability := 0;
         Control_Cap        : Rose.Capabilities.Capability := 0;
         Data_8_Cap         : Rose.Capabilities.Capability := 0;
         Data_16_Read_Cap   : Rose.Capabilities.Capability := 0;
         Data_16_Write_Cap  : Rose.Capabilities.Capability := 0;
         Base_DMA           : Rose.Words.Word_32           := 0;
         Block_Size         : Rose.Devices.Block.Block_Size_Type;
         Block_Count        : Rose.Devices.Block.Block_Address_Type;
         Get_Parameters_Cap : Rose.Capabilities.Capability := 0;
         Read_Block_Cap     : Rose.Capabilities.Capability := 0;
         Write_Block_Cap    : Rose.Capabilities.Capability := 0;
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

   function Get_Parameters_Cap
     (Drive : ATA_Drive)
      return Rose.Capabilities.Capability
   is (Drive.Get_Parameters_Cap);

   function Read_Block_Cap
     (Drive : ATA_Drive)
      return Rose.Capabilities.Capability
   is (Drive.Read_Block_Cap);

   function Write_Block_Cap
     (Drive : ATA_Drive)
      return Rose.Capabilities.Capability
   is (Drive.Write_Block_Cap);

end ATA.Drives;
