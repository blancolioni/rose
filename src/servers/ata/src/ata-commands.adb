with Rose.Devices.Port_IO;
with Rose.Console_IO;

package body ATA.Commands is

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
                  (Command => ATA_Flush,
                   Master  => Master,
                   others  => <>);
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

   ------------------------
   -- Initialize_Command --
   ------------------------

   procedure Initialize_Command
     (Item    : out ATA_Command;
      Command : ATA_Command_Type;
      Master  : Boolean;
      Use_LBA : Boolean := True)
   is
   begin
      Item := (Command => Command, Master => Master, Use_LBA => Use_LBA,
               others  => <>);
   end Initialize_Command;

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
        (Command_Port, R_Select_Drive,
         To_Select_Drive (Command.Master, Command.Use_LBA, Command.LBA));

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
         use Rose.Words;
         Data : Rose.Devices.Port_IO.Word_8_Data_Array (1 .. 6);
      begin
         Data (1) := (1, 0);
         Data (2) := (2, Command.Sector_Count);

         if Command.Use_LBA then
            declare
               LBA_28 : constant Word_32 := Word_32 (Command.LBA);
            begin
               Data (3) := (3, Word_8 (LBA_28 mod 256));
               Data (4) := (4, Word_8 (LBA_28 / 256 mod 256));
               Data (5) := (5, Word_8 (LBA_28 / 65536 mod 256));
            end;
         else
            Data (3) := (3, Command.Sector);
            Data (4) := (4, Word_8 (Command.Cylinder mod 256));
            Data (5) := (5, Word_8 (Command.Cylinder / 256));
         end if;

         Data (6) := (7, Rose.Words.Word_8 (Command.Command));

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
      Command := (Command => ATA_Identify,
                  Master  => Master,
                  Use_LBA => LBA,
                  others  => <>);
   end Set_Identify_Command;

   -----------------------------
   -- Set_Read_Sector_Command --
   -----------------------------

   procedure Set_Read_Sector_Command
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Rose.Devices.Block.Block_Address_Type)
   is
   begin
      Command := (Command      => ATA_Read_Sectors,
                  Master       => Master,
                  Use_LBA      => True,
                  Sector_Count => 1,
                  LBA          => LBA,
                  others       => <>);
   end Set_Read_Sector_Command;

   ------------------------------
   -- Set_Write_Sector_Command --
   ------------------------------

   procedure Set_Write_Sector_Command
     (Command : out ATA_Command;
      Master  : Boolean;
      LBA     : Rose.Devices.Block.Block_Address_Type)
   is
   begin
      Command := (Command      => ATA_Write_Sectors,
                  Master       => Master,
                  Use_LBA      => True,
                  Sector_Count => 1,
                  LBA          => LBA,
                  others       => <>);
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
      Mask      : ATA_Status;
      Value     : ATA_Status)
      return Boolean
   is
      X : ATA_Status := 0;
   begin
      for I in 1 .. 599 loop
         if False and then I mod 100 = 0 then
            Rose.Console_IO.Put_Line ("waiting ...");
         end if;
         X := ATA_Status (Rose.Devices.Port_IO.Port_In_8 (Data_Port, 7));
         if (X and Mask) = Value then
            return True;
         end if;
         if (X and 1) /= 0 then
            return False;
         end if;
      end loop;
      if False then
         Rose.Console_IO.Put ("wait for status: giving up; last status ");
         Rose.Console_IO.Put (Rose.Words.Word_8 (X));
         Rose.Console_IO.Put ("; expected ");
         Rose.Console_IO.Put (Rose.Words.Word_8 (Mask));
         Rose.Console_IO.Put (" ");
         Rose.Console_IO.Put (Rose.Words.Word_8 (Value));
         Rose.Console_IO.New_Line;
      end if;
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
