with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Words;

with Rose.Devices.Block;

package ATA.Commands is

   type ATA_Command_Type is new Rose.Words.Word_8;
   ATA_Read_Sectors : constant ATA_Command_Type := 16#20#;
   ATA_Write_Sectors  : constant ATA_Command_Type := 16#30#;
   ATA_Flush          : constant ATA_Command_Type := 16#37#;
   ATAPI_Identify     : constant ATA_Command_Type := 16#A1#;
   ATA_Identify       : constant ATA_Command_Type := 16#EC#;

   type ATA_Status is new Rose.Words.Word_8;

   Status_DRQ        : constant ATA_Status := 16#08#;
   Status_Busy       : constant ATA_Status := 16#80#;

   type ATA_Command is private;

   procedure Initialize_Command
     (Item    : out ATA_Command;
      Command : ATA_Command_Type;
      Master  : Boolean;
      Use_LBA : Boolean := True);

   procedure Set_Identify_Command
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Boolean);

   procedure Set_Read_Sector_Command
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Rose.Devices.Block.Block_Address_Type);

   procedure Set_Write_Sector_Command
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Rose.Devices.Block.Block_Address_Type);

   function Send_Command
     (Command      : ATA_Command;
      Command_Port : Rose.Capabilities.Capability;
      Control_Port : Rose.Capabilities.Capability;
      Data_Port    : Rose.Capabilities.Capability)
      return Boolean;

   function Wait_For_Status
     (Data_Port : Rose.Capabilities.Capability;
      Mask      : ATA_Status;
      Value     : ATA_Status)
      return Boolean;

   procedure Read_Sector
     (Data_Port    : Rose.Capabilities.Capability;
      Sector       : out System.Storage_Elements.Storage_Array);

   procedure Write_Sector
     (Data_Port    : Rose.Capabilities.Capability;
      Sector       : System.Storage_Elements.Storage_Array);

   procedure Flush
     (Command_Port : Rose.Capabilities.Capability;
      Control_Port : Rose.Capabilities.Capability;
      Data_Port    : Rose.Capabilities.Capability;
      Master       : Boolean);

private

   type Command_Register is
     (Data, Error, Sector_Count, Sector_Number,
      Cylinder_Low, Cylinder_High, Select_Drive,
      Command_Status);

   function Precomp return Command_Register renames Error;

   type ATA_Command is
      record
         Command      : ATA_Command_Type;
         Master       : Boolean;
         Use_LBA      : Boolean            := True;
         LBA          : Rose.Devices.Block.Block_Address_Type := 0;
         Data         : Rose.Words.Word_8  := 0;
         Error        : Rose.Words.Word_8  := 0;
         Sector_Count : Rose.Words.Word_8  := 0;
         Sector       : Rose.Words.Word_8  := 0;
         Cylinder     : Rose.Words.Word_16 := 0;
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
