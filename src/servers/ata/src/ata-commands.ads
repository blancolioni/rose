with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Words;

with Rose.Devices.Block;

package ATA.Commands is

   Status_Busy : constant Rose.Words.Word_8 := 16#80#;

   type ATA_Command is private;

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
      Mask      : Rose.Words.Word_8;
      Value     : Rose.Words.Word_8)
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

   type ATA_Command is array (Command_Register) of Rose.Words.Word_8;

end ATA.Commands;
