with Rose.Capabilities;
with Rose.Words;

package ATA.Commands is

   Status_Busy : constant Rose.Words.Word_8 := 16#80#;

   type ATA_Command is private;

   procedure Identify
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Boolean);

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

private

   type Command_Register is
     (Data, Error, Sector_Count, Sector_Number,
      Cylinder_Low, Cylinder_High, Select_Drive,
      Command_Status);

   function Precomp return Command_Register renames Error;

   type ATA_Command is array (Command_Register) of Rose.Words.Word_8;

end ATA.Commands;
