with Rose.Console_IO;
with Rose.Devices.Port_IO;
with Rose.Invocation;
with Rose.Objects;
with Rose.System_Calls.Server;

with ATA.Commands;

package body ATA.Drives is

   Drive_Table : array (ATA_Drive_Index) of aliased ATA_Drive_Record;

   function Get (Index : ATA_Drive_Index) return ATA_Drive
   is (Drive_Table (Index)'Access);

   Id_Buffer : array (1 .. 256) of Rose.Words.Word_16;

   function Identify
     (Drive : ATA_Drive;
      Atapi : Boolean)
      return Boolean;

   procedure Put (Drive : ATA_Drive);

   --------------
   -- Identify --
   --------------

   function Identify
     (Drive : ATA_Drive;
      Atapi : Boolean)
      return Boolean
   is
      use Rose.Words;
      Command : ATA.Commands.ATA_Command;
   begin

      ATA.Commands.Initialize_Command
        (Item    => Command,
         Command => (if Atapi
                     then ATA.Commands.ATAPI_Identify
                     else ATA.Commands.ATA_Identify),
         Master  => Drive.Index in 0 | 2,
         Use_LBA => False);

      if not ATA.Commands.Send_Command (Drive, Command) then
         return False;
      end if;

      if not ATA.Commands.Poll_Status_Bits (Drive, DRQ => True) then
         return False;
      end if;

      for I in 1 .. 256 loop
         declare
            D : constant Rose.Words.Word_16 :=
                  Rose.Devices.Port_IO.Port_In_16 (Drive.Data_16_Read_Cap);
         begin
            Id_Buffer (I) := D;
         end;
      end loop;

      Rose.Console_IO.Put ("ata: ");
      Put (Drive);
      Rose.Console_IO.Put (": ");

      if Atapi then
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
               Drive.Block_Count :=
                 Rose.Devices.Block.Block_Address_Type (Sector_Count);
            end if;
         end;

         Rose.Console_IO.Put ("size: ");
         declare
            Size : constant Word_64 :=
                     Word_64 (Drive.Block_Count)
                     * Word_64 (Drive.Block_Size);
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

      Drive.Listening := True;
      Drive.Atapi := Atapi;

      if Drive.Atapi then
         Drive.Block_Size := 2048;
         Drive.Block_Count := 4096;  --  oh, dear
      end if;

      Drive.Get_Parameters_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Cap  => Create_Endpoint_Cap,
           Endpoint_Id =>
             Rose.Devices.Block.Get_Device_Parameters_Endpoint,
           Identifier  =>
             Rose.Objects.Capability_Identifier (Drive.Index));

      Drive.Read_Block_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Cap  => Create_Endpoint_Cap,
           Endpoint_Id =>
             Rose.Devices.Block.Read_Blocks_Endpoint,
           Identifier  =>
             Rose.Objects.Capability_Identifier (Drive.Index));

      if not Drive.Atapi then
         Drive.Write_Block_Cap :=
           Rose.System_Calls.Server.Create_Endpoint
             (Create_Cap  => Create_Endpoint_Cap,
              Endpoint_Id =>
                Rose.Devices.Block.Write_Blocks_Endpoint,
              Identifier  =>
                Rose.Objects.Capability_Identifier (Drive.Index));

      end if;

      Drive.Interrupt_Cap :=
        Rose.System_Calls.Server.Create_Receive_Cap
          (Create_Cap   => Create_Endpoint_Cap,
           Endpoint_Id  => Primary_Endpoint);

      return True;
   end Identify;

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
      Drive    : constant ATA_Drive := Drive_Table (Index)'Access;
   begin
      Drive.all :=
        ATA_Drive_Record'
          (Initialized        => True,
           Index              => Index,
           Native             => Is_Native,
           Command_Cap        => Command_Cap,
           Control_Cap        => Control_Cap,
           Data_8_Cap         => Data_Cap_8,
           Data_16_Read_Cap   => Data_Read_Cap_16,
           Data_16_Write_Cap  => Data_Write_Cap_16,
           Interrupt_Cap      => Rose.Capabilities.Null_Capability,
           Base_DMA           => Base_DMA,
           Block_Size         => 512,
           Block_Count        => 0,
           others             => <>);

      for Check_Atapi in Boolean loop
         if Identify (Drive, Check_Atapi) then
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

   ---------
   -- Log --
   ---------

   procedure Log
     (Drive   : ATA_Drive;
      Message : String)
   is
      use Rose.Console_IO;
   begin
      Put ("ata: hd");
      Put (Natural (Drive.Index));
      Put (": ");
      Put (Message);
      New_Line;
   end Log;

   ---------
   -- Put --
   ---------

   procedure Put (Drive : ATA_Drive) is
   begin
      Rose.Console_IO.Put ("hd");
      Rose.Console_IO.Put (Natural (Drive.Index));
   end Put;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Drive : ATA_Drive)
   is
   begin
      ATA.Commands.Send_Control (Drive, 16#04#);
      ATA.Commands.Send_Control (Drive, 16#00#);
      if not ATA.Commands.Poll_Status_Bits (Drive) then
         ATA.Drives.Log (Drive, "reset failed");
         ATA.Drives.Set_Dead (Drive);
         return;
      end if;
      Drive.Dead := False;
   end Reset;

   --------------
   -- Set_Dead --
   --------------

   procedure Set_Dead (Drive : ATA_Drive) is
   begin
      Drive.Dead := True;
   end Set_Dead;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Drive  : ATA_Drive;
      Status : ATA_Status)
   is
   begin
      Drive.Status := Status;
   end Set_Status;

   ------------------------
   -- Wait_For_Interrupt --
   ------------------------

   function Wait_For_Interrupt
     (Drive : ATA_Drive)
      return Boolean
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin

      Params := (others => <>);
      Params.Control.Flags (Rose.Invocation.Receive) := True;
      Params.Control.Flags (Rose.Invocation.Block) := True;
      Params.Control.Flags (Rose.Invocation.Recv_Words) := True;
      Params.Control.Last_Recv_Word := 0;
      Params.Cap := Drive.Interrupt_Cap;

      Rose.System_Calls.Invoke_Capability (Params);

      case Params.Endpoint is
         when Primary_Endpoint =>
            declare
               Status : constant ATA_Status :=
                          ATA_Status
                            (Rose.Devices.Port_IO.Port_In_8
                               (Data_8_Port (Drive), 7));
            begin
               if (Status and Status_Error) /= 0 then
                  Log (Drive, "interrupt: error status");
                  return False;
               end if;
            end;

         when Secondary_Endpoint =>
            Log (Drive, "received secondary interrupt");

         when others =>
            Log (Drive, "ignoring message while waiting for interrupt");
      end case;

      return True;
   end Wait_For_Interrupt;

end ATA.Drives;
