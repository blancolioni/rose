with Rose.Console_IO;
with Rose.Devices.Port_IO;

with ATA.Commands;

package body ATA.Drives is

   Drive_Table : array (ATA_Drive_Index) of aliased ATA_Drive_Record;

   function Get (Index : ATA_Drive_Index) return ATA_Drive
   is (Drive_Table (Index)'Access);

   Id_Buffer : array (1 .. 256) of Rose.Words.Word_16;

   ----------------------
   -- Initialize_Drive --
   ----------------------

   procedure Initialize_Drive
     (Index             : ATA_Drive_Index;
      Command_Cap       : Rose.Capabilities.Capability;
      Control_Cap       : Rose.Capabilities.Capability;
      Data_Cap_8        : Rose.Capabilities.Capability;
      Data_Read_Cap_16  : Rose.Capabilities.Capability;
      Data_Write_Cap_16 : Rose.Capabilities.Capability;
      Base_DMA          : Rose.Words.Word_32;
      Is_Native         : Boolean)
   is
      use Rose.Words;
      Identify : ATA.Commands.ATA_Command;
   begin
      Drive_Table (Index) :=
        ATA_Drive_Record'
          (Initialized       => True,
           Listening         => False,
           Dead              => False,
           Atapi             => False,
           Native            => Is_Native,
           Command_Cap       => Command_Cap,
           Control_Cap       => Control_Cap,
           Data_8_Cap        => Data_Cap_8,
           Data_16_Read_Cap  => Data_Read_Cap_16,
           Data_16_Write_Cap => Data_Write_Cap_16,
           Base_DMA          => Base_DMA,
           Block_Size        => 512,
           Block_Count       => 0);

      for Check_Atapi in Boolean loop
         ATA.Commands.Initialize_Command
           (Item => Identify,
            Command => (if Check_Atapi
                        then ATA.Commands.ATAPI_Identify
                        else ATA.Commands.ATA_Identify),
            Master  => Index in 0 | 2,
            Use_LBA => False);

         if ATA.Commands.Send_Command
           (Identify, Command_Cap, Control_Cap, Data_Cap_8)
           and then
             ATA.Commands.Wait_For_Status
               (Data_Cap_8, ATA.Commands.Status_DRQ, ATA.Commands.Status_DRQ)
         then
            for I in 1 .. 256 loop
               declare
                  D : constant Rose.Words.Word_16 :=
                        Rose.Devices.Port_IO.Port_In_16 (Data_Read_Cap_16);
               begin
                  Id_Buffer (I) := D;
               end;
            end loop;

            Rose.Console_IO.Put ("ata: hd");
            Rose.Console_IO.Put (Natural (Index));
            Rose.Console_IO.Put (": ");

            if Check_Atapi then
               Rose.Console_IO.Put ("atapi");
            else
               Rose.Console_IO.Put (Natural (Id_Buffer (2)));
               Rose.Console_IO.Put ("/");
               Rose.Console_IO.Put (Natural (Id_Buffer (4)));
               Rose.Console_IO.Put ("/");
               Rose.Console_IO.Put (Natural (Id_Buffer (7)));

               Rose.Console_IO.Put (": ");
               if (Id_Buffer (84) and 2 ** 10) /= 0 then
                  Rose.Console_IO.Put ("lba48 ");
               end if;

               declare
                  Sector_Count : constant Word_32 :=
                                   Word_32 (Id_Buffer (61))
                                   + 65536 * Word_32 (Id_Buffer (62));
               begin
                  if Sector_Count > 0 then
                     Rose.Console_IO.Put ("lba28 ");
                     Rose.Console_IO.Put (Natural (Sector_Count));
                     Rose.Console_IO.Put (" ");
                     Drive_Table (Index).Block_Count :=
                       Rose.Devices.Block.Block_Address_Type (Sector_Count);
                  end if;
               end;

               Rose.Console_IO.Put ("size: ");
               declare
                  Size : constant Word_64 :=
                           Word_64 (Drive_Table (Index).Block_Count)
                           * Word_64 (Drive_Table (Index).Block_Size);
               begin
                  if Size < 2 ** 32 then
                     Rose.Console_IO.Put (Natural (Size / 2 ** 20));
                     Rose.Console_IO.Put ("M");
                  else
                     Rose.Console_IO.Put (Natural (Size / 2 ** 30));
                     Rose.Console_IO.Put ("G");
                  end if;
               end;
            end if;

            Rose.Console_IO.Put (" ");

            for I in 24 .. 42 loop
               declare
                  S : constant String (1 .. 2) :=
                        (Character'Val (Id_Buffer (I) / 256),
                         Character'Val (Id_Buffer (I) mod 256));
               begin
                  for Ch of S loop
                     if Ch in ' ' .. '~' then
                        Rose.Console_IO.Put (Ch);
                     else
                        Rose.Console_IO.Put ('.');
                     end if;
                  end loop;
               end;
            end loop;

            Rose.Console_IO.New_Line;

            Drive_Table (Index).Listening := True;
            Drive_Table (Index).Atapi := Check_Atapi;
            return;
         end if;
      end loop;

      Rose.Console_IO.Put ("unable to initialise hd");
      Rose.Console_IO.Put (Natural (Index));
      Rose.Console_IO.New_Line;
      Drive_Table (Index).Dead := True;
   end Initialize_Drive;

   -------------
   -- Is_Dead --
   -------------

   function Is_Dead (Drive : ATA_Drive) return Boolean is
   begin
      return Drive.Dead;
   end Is_Dead;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized (Drive : ATA_Drive) return Boolean is
   begin
      return Drive.Initialized;
   end Is_Initialized;

   ------------------
   -- Is_Listening --
   ------------------

   function Is_Listening (Drive : ATA_Drive) return Boolean is
   begin
      return Drive.Listening;
   end Is_Listening;

   ----------------
   -- Read_Block --
   ----------------

   procedure Read_Block
     (Index   : ATA_Drive_Index;
      Address : Rose.Devices.Block.Block_Address_Type;
      Buffer  : out System.Storage_Elements.Storage_Array)
   is
      Command : ATA.Commands.ATA_Command;
      Drive   : ATA_Drive_Record renames Drive_Table (Index);
   begin

      if Drive.Dead then
         Rose.Console_IO.Put ("ata: drive ");
         Rose.Console_IO.Put (Natural (Index));
         Rose.Console_IO.Put (" is dead");
         return;
      end if;

      ATA.Commands.Set_Read_Sector_Command
        (Command => Command,
         Master  => Index in 0 | 2,
         LBA     => Address);

      if ATA.Commands.Send_Command
        (Command      => Command,
         Command_Port => Drive.Command_Cap,
         Control_Port => Drive.Control_Cap,
         Data_Port    => Drive.Data_8_Cap)
      then
         if not ATA.Commands.Wait_For_Status
           (Drive.Data_8_Cap, ATA.Commands.Status_Busy, 0)
         then
            Drive_Table (Index).Dead := True;
            return;
         end if;

         ATA.Commands.Read_Sector
           (Data_Port    => Drive.Data_16_Read_Cap,
            Sector       => Buffer);
      else
         Rose.Console_IO.Put ("ata: sending to drive ");
         Rose.Console_IO.Put (Natural (Index));
         Rose.Console_IO.Put (" failed");
      end if;
   end Read_Block;

   -----------------
   -- Write_Block --
   -----------------

   procedure Write_Block
     (Index   : ATA_Drive_Index;
      Address : Rose.Devices.Block.Block_Address_Type;
      Buffer  : System.Storage_Elements.Storage_Array)
   is
      Command : ATA.Commands.ATA_Command;
      Drive   : ATA_Drive_Record renames Drive_Table (Index);
   begin

      if Drive.Dead then
         Rose.Console_IO.Put ("ata: drive ");
         Rose.Console_IO.Put (Natural (Index));
         Rose.Console_IO.Put (" is dead");
         return;
      end if;

      ATA.Commands.Set_Write_Sector_Command
        (Command => Command,
         Master  => Index in 0 | 2,
         LBA     => Address);

      if ATA.Commands.Send_Command
        (Command      => Command,
         Command_Port => Drive.Command_Cap,
         Control_Port => Drive.Control_Cap,
         Data_Port    => Drive.Data_8_Cap)
      then
         if not ATA.Commands.Wait_For_Status
           (Drive.Data_8_Cap, ATA.Commands.Status_Busy, 0)
         then
            Drive_Table (Index).Dead := True;
            return;
         end if;

         ATA.Commands.Write_Sector
           (Data_Port    => Drive.Data_16_Write_Cap,
            Sector       => Buffer);
         ATA.Commands.Flush
           (Command_Port => Drive.Command_Cap,
            Control_Port => Drive.Control_Cap,
            Data_Port    => Drive.Data_8_Cap,
            Master       => Index in 0 | 2);
      else
         Rose.Console_IO.Put ("ata: sending to drive ");
         Rose.Console_IO.Put (Natural (Index));
         Rose.Console_IO.Put (" failed");
      end if;
   end Write_Block;

end ATA.Drives;
