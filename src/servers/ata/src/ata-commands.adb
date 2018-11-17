with Rose.Devices.Port_IO;
with Rose.Console_IO;

package body ATA.Commands is

   ATA_Read_Sectors  : constant Rose.Words.Word_8 := 16#20#;
   ATA_Write_Sectors : constant Rose.Words.Word_8 := 16#30#;
   ATA_Flush         : constant Rose.Words.Word_8 := 16#E7#;
   ATA_Identify      : constant Rose.Words.Word_8 := 16#EC#;

   function To_Select_Drive
     (Master  : Boolean;
      LBA     : Boolean;
      Address : Rose.Devices.Block.Block_Address_Type := 0)
      return Rose.Words.Word_8;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Command_Port : Rose.Capabilities.Capability;
      Control_Port : Rose.Capabilities.Capability;
      Data_Port    : Rose.Capabilities.Capability;
      Master       : Boolean)
   is
      Command : constant ATA_Command :=
                  (Command_Status => ATA_Flush,
                   Select_Drive   => To_Select_Drive (Master, True),
                   others         => 0);
   begin
      if not Send_Command
        (Command      => Command,
         Command_Port => Command_Port,
         Control_Port => Control_Port,
         Data_Port    => Data_Port)
      then
         Rose.Console_IO.Put_Line ("cannot flush drive");
      end if;
   end Flush;

   -----------------
   -- Read_Sector --
   -----------------

   procedure Read_Sector
     (Data_Port    : Rose.Capabilities.Capability;
      Sector       : out System.Storage_Elements.Storage_Array)
   is
      use System.Storage_Elements;
      Index : Storage_Offset := Sector'First - 1;
   begin
      for I in 1 .. 256 loop
         declare
            use Rose.Words;
            D : constant Rose.Words.Word_16 :=
                  Rose.Devices.Port_IO.Port_In_16 (Data_Port);
         begin
            Index := Index + 1;
            Sector (Index) := Storage_Element (D mod 256);
            Index := Index + 1;
            Sector (Index) := Storage_Element (D / 256);
         end;
      end loop;

   end Read_Sector;

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

   --------------------------
   -- Set_Identify_Command --
   --------------------------

   procedure Set_Identify_Command
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Boolean)
   is
   begin
      Command := (Command_Status => ATA_Identify,
                  Select_Drive   => To_Select_Drive (Master, LBA),
                  others         => 0);
   end Set_Identify_Command;

   -----------------------------
   -- Set_Read_Sector_Command --
   -----------------------------

   procedure Set_Read_Sector_Command
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Rose.Devices.Block.Block_Address_Type)
   is
      use Rose.Words;
      LBA_28 : constant Word_32 := Word_32 (LBA);
   begin
      Command := (Command_Status => ATA_Read_Sectors,
                  Sector_Count   => 1,
                  Sector_Number  => Word_8 (LBA_28 mod 256),
                  Cylinder_Low   => Word_8 (LBA_28 / 256 mod 256),
                  Cylinder_High  => Word_8 (LBA_28 / 65536 mod 256),
                  Select_Drive   => To_Select_Drive (Master, True, LBA),
                  others         => 0);
   end Set_Read_Sector_Command;

   ------------------------------
   -- Set_Write_Sector_Command --
   ------------------------------

   procedure Set_Write_Sector_Command
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Rose.Devices.Block.Block_Address_Type)
   is
      use Rose.Words;
      LBA_28 : constant Word_32 := Word_32 (LBA);
   begin
      Command := (Command_Status => ATA_Write_Sectors,
                  Sector_Count   => 1,
                  Sector_Number  => Word_8 (LBA_28 mod 256),
                  Cylinder_Low   => Word_8 (LBA_28 / 256 mod 256),
                  Cylinder_High  => Word_8 (LBA_28 / 65536 mod 256),
                  Select_Drive   => To_Select_Drive (Master, True, LBA),
                  others         => 0);
   end Set_Write_Sector_Command;

   ---------------------
   -- To_Select_Drive --
   ---------------------

   function To_Select_Drive
     (Master  : Boolean;
      LBA     : Boolean;
      Address : Rose.Devices.Block.Block_Address_Type := 0)
      return Rose.Words.Word_8
   is
      use Rose.Words;
   begin
      return Boolean'Pos (LBA) * 2 ** 6
        + Boolean'Pos (not Master) * 2 ** 4
        + 2 ** 7 + 2 ** 5
        + Word_8 (Word_32 (Address) / 2 ** 24);
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
      use Rose.Words;
      X : Word_8 := 0;
   begin
      for I in 1 .. 599 loop
         if I mod 100 = 0 then
            Rose.Console_IO.Put_Line ("waiting ...");
         end if;
         X := Rose.Devices.Port_IO.Port_In_8 (Data_Port, 7);
         if (X and Mask) = Value then
            return True;
         end if;
         if (X and 1) /= 0 then
            Rose.Console_IO.Put_Line ("error bit set");
            return False;
         end if;
      end loop;
      Rose.Console_IO.Put ("wait for status: giving up; last status ");
      Rose.Console_IO.Put (X);
      Rose.Console_IO.Put ("; expected ");
      Rose.Console_IO.Put (Mask);
      Rose.Console_IO.Put (" ");
      Rose.Console_IO.Put (Value);
      Rose.Console_IO.New_Line;
      return False;
   end Wait_For_Status;

   ------------------
   -- Write_Sector --
   ------------------

   procedure Write_Sector
     (Data_Port : Rose.Capabilities.Capability;
      Sector    : System.Storage_Elements.Storage_Array)
   is
      use System.Storage_Elements;
      Index : Storage_Offset := Sector'First;
   begin
      for I in 1 .. 256 loop
         declare
            use Rose.Words;
            D : constant Word_16 :=
                  Word_16 (Sector (Index)) +
                  256 * Word_16 (Sector (Index + 1));
         begin
            Rose.Devices.Port_IO.Port_Out_16 (Data_Port, D);
            Index := Index + 2;
         end;
      end loop;
   end Write_Sector;

end ATA.Commands;
