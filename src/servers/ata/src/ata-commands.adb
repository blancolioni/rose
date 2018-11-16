with Rose.Devices.Port_IO;
with Rose.Console_IO;

package body ATA.Commands is

   ATA_Identify : constant Rose.Words.Word_8 := 16#EC#;

   function To_Select_Drive
     (Master : Boolean;
      LBA    : Boolean)
      return Rose.Words.Word_8;

   --------------
   -- Identify --
   --------------

   procedure Identify
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Boolean)
   is
   begin
      Command := (Command_Status => ATA_Identify,
                  Select_Drive   => To_Select_Drive (Master, LBA),
                  others         => 0);
   end Identify;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Command      : ATA_Command;
      Command_Port : Rose.Capabilities.Capability;
      Control_Port : Rose.Capabilities.Capability;
      Data_Port    : Rose.Capabilities.Capability)
      return Boolean
   is
   begin

      Rose.Devices.Port_IO.Port_Out_8
        (Command_Port,
         Command_Register'Pos (Select_Drive),
         Command (Select_Drive));

      if False then
         if not Wait_For_Status (Data_Port, Status_Busy, 0) then
            return False;
         end if;
      end if;

      if False then
         Rose.Devices.Port_IO.Port_Out_8
           (Port   => Control_Port,
            Offset => 0,
            Value  => 16#08#);
      end if;

      declare
         Data : Rose.Devices.Port_IO.Word_8_Data_Array (1 .. 6);
      begin
         Data (1) := (1, Command (Precomp));
         Data (2) := (2, Command (Sector_Count));
         Data (3) := (3, Command (Sector_Number));
         Data (4) := (4, Command (Cylinder_Low));
         Data (5) := (5, Command (Cylinder_High));
         Data (6) := (7, Command (Command_Status));

         Rose.Devices.Port_IO.Port_Out_8
           (Port => Command_Port,
            Data => Data);
      end;

      return True;
   end Send_Command;

   ---------------------
   -- To_Select_Drive --
   ---------------------

   function To_Select_Drive
     (Master : Boolean;
      LBA    : Boolean)
      return Rose.Words.Word_8
   is
      use Rose.Words;
   begin
      return Boolean'Pos (LBA) * 2 ** 6
        + Boolean'Pos (not Master) * 2 ** 4
        + 2 ** 7 + 2 ** 5;
   end To_Select_Drive;

   ---------------------
   -- Wait_For_Status --
   ---------------------

   function Wait_For_Status
     (Data_Port : Rose.Capabilities.Capability;
      Mask      : Rose.Words.Word_8;
      Value     : Rose.Words.Word_8)
     return Boolean
   is
   begin
      for I in 1 .. 5 loop
         declare
            use Rose.Words;
            X : constant Word_8 :=
                  Rose.Devices.Port_IO.Port_In_8 (Data_Port, 7);
         begin
            if (X and Mask) = Value then
               return True;
            end if;
         end;
      end loop;
      Rose.Console_IO.Put_Line ("wait for status: giving up");
      return False;
   end Wait_For_Status;

end ATA.Commands;
