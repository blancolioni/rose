with System.Storage_Elements;

with Rose.Words;

with Rose.Devices.Block;

with ATA.Drives;

package ATA.Commands is

   type ATA_Command_Type is new Rose.Words.Word_8;
   ATA_Read_Sector    : constant ATA_Command_Type := 16#20#;
   ATA_Write_Sector   : constant ATA_Command_Type := 16#30#;
   ATA_Read_Multiple  : constant ATA_Command_Type := 16#C4#;
   ATA_Write_Multiple : constant ATA_Command_Type := 16#C5#;
   ATA_Set_Multiple   : constant ATA_Command_Type := 16#C6#;

   ATA_Flush          : constant ATA_Command_Type := 16#37#;

   ATA_Identify       : constant ATA_Command_Type := 16#EC#;

   ATAPI_Packet       : constant ATA_Command_Type := 16#A0#;
   ATAPI_Identify     : constant ATA_Command_Type := 16#A1#;
   ATAPI_Read         : constant ATA_Command_Type := 16#A8#;

   type ATA_Command is private;

   procedure Initialize_Command
     (Item    : out ATA_Command;
      Command : ATA_Command_Type;
      Master  : Boolean;
      Use_LBA : Boolean := True);

   function Send_Command
     (Drive        : ATA.Drives.ATA_Drive;
      Command      : ATA_Command)
      return Boolean;

   procedure Send_Control
     (Drive : ATA.Drives.ATA_Drive;
      Value : Rose.Words.Word_8);

   function Wait_For_Status
     (Drive     : ATA.Drives.ATA_Drive;
      Mask      : ATA.Drives.ATA_Status;
      Value     : ATA.Drives.ATA_Status)
      return Boolean;

   function Check_Drive_OK
     (Drive     : ATA.Drives.ATA_Drive)
      return Boolean;

   procedure Read_Sectors
     (Drive   : ATA.Drives.ATA_Drive;
      Address : Rose.Devices.Block.Block_Address_Type;
      Count   : Positive;
      Sectors : out System.Storage_Elements.Storage_Array;
      Success : out Boolean);

   procedure Write_Sectors
     (Drive   : ATA.Drives.ATA_Drive;
      Address : Rose.Devices.Block.Block_Address_Type;
      Count   : Positive;
      Sectors : System.Storage_Elements.Storage_Array;
      Success : out Boolean);

   procedure Flush
     (Drive : ATA.Drives.ATA_Drive);

private

   type Command_Register is
     (Data, Error, Sector_Count, Sector_Number,
      Cylinder_Low, Cylinder_High, Select_Drive,
      Command_Status);

   function Precomp return Command_Register renames Error;

   type ATA_Command is
      record
         Command        : ATA_Command_Type;
         Master         : Boolean;
         Use_LBA        : Boolean            := True;
         Atapi          : Boolean            := False;
         LBA            : Rose.Devices.Block.Block_Address_Type := 0;
         Data           : Rose.Words.Word_8  := 0;
         Error          : Rose.Words.Word_8  := 0;
         Sector_Count   : Rose.Words.Word_8  := 0;
         Sector         : Rose.Words.Word_8  := 0;
         Cylinder       : Rose.Words.Word_16 := 0;
         Max_Byte_Count : Rose.Words.Word_16 := 0;
      end record;

   R_Control       : constant Rose.Words.Word_8 := 0;
   R_Error         : constant Rose.Words.Word_8 := 1;
   R_Features      : constant Rose.Words.Word_8 := 1;
   R_Sector_Count  : constant Rose.Words.Word_8 := 2;
   R_Sector_Number : constant Rose.Words.Word_8 := 3;
   R_Cylinder_Low  : constant Rose.Words.Word_8 := 4;
   R_Cylinder_High : constant Rose.Words.Word_8 := 5;
   R_Select_Drive  : constant Rose.Words.Word_8 := 6;
   R_Command       : constant Rose.Words.Word_8 := 7;
   R_Status        : constant Rose.Words.Word_8 := 7;

end ATA.Commands;
