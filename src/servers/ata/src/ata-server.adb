with System.Storage_Elements;

with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Rose.Invocation;
with Rose.Objects;
with Rose.Words;                       use Rose.Words;

with Rose.Devices.PCI.Client;
with Rose.Console_IO;

with Rose.Devices.Block;
with Rose.Interfaces.Block_Device;
with Rose.Interfaces.Ata;

with ATA.Commands;
with ATA.Drives;

package body ATA.Server is

   ATA_Native_0    : constant Rose.Words.Word_8 := 2#0000_0001#;
   ATA_Native_1    : constant Rose.Words.Word_8 := 2#0000_0100#;

   Device_Cap  : Rose.Capabilities.Capability;

   type Vendor_Device_Record is
      record
         Vendor : Rose.Words.Word_32;
         Device : Rose.Words.Word_32;
      end record;

   Known_Devices : constant array (Positive range <>) of
     Vendor_Device_Record :=
       (1 => (16#8086#, 16#7111#),
        2 => (16#8086#, 16#7010#));

   procedure Get_Parameters
     (Identifier    : Rose.Objects.Capability_Identifier;
      Block_Size    : out Rose.Devices.Block.Block_Size_Type;
      Block_Count   : out Rose.Devices.Block.Block_Address_Type);

   procedure Get_Interface
     (Interface_Id       : Rose.Words.Word;
      Get_Parameters_Cap : out Rose.Capabilities.Capability;
      Read_Block_Cap     : out Rose.Capabilities.Capability;
      Write_Block_Cap    : out Rose.Capabilities.Capability);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      use type Rose.Capabilities.Capability;
   begin

      Device_Cap := Rose.Capabilities.Null_Capability;

      for Rec of Known_Devices loop
         Device_Cap :=
           Rose.System_Calls.Client.Get_Capability
             (PCI_Cap, (Rec.Vendor, Rec.Device));
         exit when Device_Cap /= Rose.Capabilities.Null_Capability;
      end loop;

      if Device_Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put_Line
           ("ata: no devices");
      else
         Rose.Console_IO.Put ("ata: probing PCI IDE devices with device cap ");
         Rose.Console_IO.Put (Rose.Words.Word_8 (Device_Cap));
         Rose.Console_IO.New_Line;

         declare
            use Rose.Devices.PCI;
            use Rose.Devices.PCI.Client;
            Class_Id     : constant Word_8 := Get (Device_Cap, R_Class);
            Sub_Class_Id : constant Word_8 := Get (Device_Cap, R_Sub_Class);
            Prog_IF      : constant Word_8 := Get (Device_Cap, R_Prog_IF);
            Is_IDE       : constant Boolean :=
                             Class_Id = PCI_Mass_Storage
                                 and then Sub_Class_Id = PCI_IDE;
            Has_Native_0 : constant Boolean :=
                             (Prog_IF and ATA_Native_0) /= 0;
            Has_Native_1 : constant Boolean :=
                             (Prog_IF and ATA_Native_1) /= 0;
--              IRQ          : constant Word_8 :=
--                               Get (Device_Cap, R_Interrupt_Line);
            Base_DMA     : constant Word_32 :=
                             Get (Device_Cap, R_BAR_4)
                               and PCI_Bar_IO_Mask;

         begin
            if not Is_IDE or else Has_Native_0 then
               Rose.Console_IO.Put_Line
                 ("found native IDE on channel 0");
            else
               Rose.Console_IO.Put_Line
                 ("found compatability IDE on channel 0");
               ATA.Drives.Initialize_Drive
                 (Index             => 0,
                  Command_Cap       => Command_0_Cap,
                  Control_Cap       => Control_0_Cap,
                  Data_Cap_8        => Data_0_Cap_8,
                  Data_Read_Cap_16  => Data_0_Cap_Read_16,
                  Data_Write_Cap_16 => Data_0_Cap_Write_16,
                  Base_DMA          => Base_DMA,
                  Is_Native         => False);
               ATA.Drives.Initialize_Drive
                 (Index             => 1,
                  Command_Cap       => Command_0_Cap,
                  Control_Cap       => Control_0_Cap,
                  Data_Cap_8        => Data_0_Cap_8,
                  Data_Read_Cap_16  => Data_0_Cap_Read_16,
                  Data_Write_Cap_16 => Data_0_Cap_Write_16,
                  Base_DMA          => Base_DMA,
                  Is_Native         => False);
            end if;

            if False then
               if not Is_IDE or else Has_Native_1 then
                  Rose.Console_IO.Put_Line
                    ("found native IDE on channel 1");
               else
                  Rose.Console_IO.Put_Line
                    ("found compatability IDE on channel 1");
                  ATA.Drives.Initialize_Drive
                    (Index               => 2,
                     Command_Cap         => Command_1_Cap,
                     Control_Cap         => Control_1_Cap,
                     Data_Cap_8          => Data_1_Cap_8,
                     Data_Read_Cap_16    => Data_1_Cap_Read_16,
                     Data_Write_Cap_16   => Data_1_Cap_Write_16,
                     Base_DMA            => Base_DMA,
                     Is_Native           => False);
               end if;
            end if;
         end;

      end if;

--        Receive_Cap :=
--          Rose.System_Calls.Server.Create_Endpoint
--            (Create_Cap,

   end Create_Server;

   -------------------
   -- Get_Interface --
   -------------------

   procedure Get_Interface
     (Interface_Id       : Rose.Words.Word;
      Get_Parameters_Cap : out Rose.Capabilities.Capability;
      Read_Block_Cap     : out Rose.Capabilities.Capability;
      Write_Block_Cap    : out Rose.Capabilities.Capability)
   is
      Drive_Index : constant ATA.Drives.ATA_Drive_Index :=
                      ATA.Drives.ATA_Drive_Index (Interface_Id);
      Drive       : constant ATA.Drives.ATA_Drive :=
                      ATA.Drives.Get (Drive_Index);
   begin
      Get_Parameters_Cap :=
        ATA.Drives.Get_Parameters_Cap (Drive);
      Read_Block_Cap :=
        ATA.Drives.Read_Block_Cap (Drive);
      Write_Block_Cap :=
        ATA.Drives.Write_Block_Cap (Drive);

      Rose.Console_IO.Put ("ata: caps for hd");
      Rose.Console_IO.Put (Natural (Interface_Id));
      Rose.Console_IO.Put (": ");
      Rose.Console_IO.Put (Rose.Words.Word_8 (Get_Parameters_Cap));
      Rose.Console_IO.Put (" ");
      Rose.Console_IO.Put (Rose.Words.Word_8 (Read_Block_Cap));
      Rose.Console_IO.Put (" ");
      Rose.Console_IO.Put (Rose.Words.Word_8 (Write_Block_Cap));
      Rose.Console_IO.New_Line;

   end Get_Interface;

   --------------------
   -- Get_Parameters --
   --------------------

   procedure Get_Parameters
     (Identifier    : Rose.Objects.Capability_Identifier;
      Block_Size    : out Rose.Devices.Block.Block_Size_Type;
      Block_Count   : out Rose.Devices.Block.Block_Address_Type)
   is
      Drive_Index : constant ATA.Drives.ATA_Drive_Index :=
                      ATA.Drives.ATA_Drive_Index (Identifier);
      Drive       : constant ATA.Drives.ATA_Drive :=
                      ATA.Drives.Get (Drive_Index);
   begin
      Block_Size := ATA.Drives.Block_Size (Drive);
      Block_Count := ATA.Drives.Block_Count (Drive);
   end Get_Parameters;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Create_Endpoint_Cap);
      Params      : aliased Rose.Invocation.Invocation_Record;
      Reply       : aliased Rose.Invocation.Invocation_Record;
   begin

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Get_Interface_Endpoint);

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Block_Device.Get_Parameters_Endpoint);

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Ata.Get_Device_Endpoint);

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Block_Device.Read_Blocks_Endpoint);
      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Block_Device.Write_Blocks_Endpoint);

      loop
         Params := (others => <>);
         Params.Control.Flags (Rose.Invocation.Receive) := True;
         Params.Control.Flags (Rose.Invocation.Block) := True;
         Params.Control.Flags (Rose.Invocation.Recv_Words) := True;
         Params.Control.Flags (Rose.Invocation.Recv_Buffer) := True;
         Params.Control.Last_Recv_Word :=
           Rose.Invocation.Parameter_Word_Index'Last;
         Params.Cap := Receive_Cap;

         Rose.System_Calls.Invoke_Capability (Params);

         Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         case Params.Endpoint is
            when Rose.Interfaces.Get_Interface_Endpoint =>
               declare
                  Get_Parameters_Cap : Rose.Capabilities.Capability;
                  Read_Block_Cap     : Rose.Capabilities.Capability;
                  Write_Block_Cap    : Rose.Capabilities.Capability;
               begin
                  Get_Interface (Rose.Words.Word (Params.Identifier),
                                 Get_Parameters_Cap,
                                 Read_Block_Cap,
                                 Write_Block_Cap);
                  Rose.System_Calls.Send_Cap (Reply, Get_Parameters_Cap);
                  Rose.System_Calls.Send_Cap (Reply, Read_Block_Cap);
                  Rose.System_Calls.Send_Cap (Reply, Write_Block_Cap);
               end;

            when Rose.Interfaces.Block_Device.Get_Parameters_Endpoint =>
               declare
                  Block_Size    : Rose.Devices.Block.Block_Size_Type;
                  Block_Count   : Rose.Devices.Block.Block_Address_Type;
               begin
                  Get_Parameters (Params.Identifier, Block_Size, Block_Count);

                  Rose.System_Calls.Send_Word
                    (Reply, Rose.Words.Word_64 (Block_Count));
                  Rose.System_Calls.Send_Word
                    (Reply, Rose.Words.Word_32 (Block_Size));

               end;

            when Rose.Interfaces.Block_Device.Read_Blocks_Endpoint =>
               declare
                  use Rose.Interfaces.Block_Device;
                  use System.Storage_Elements;
                  Buffer : Storage_Array (1 .. Params.Buffer_Length);
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Params.Buffer_Address;
                  Block_Address : constant Block_Address_Type :=
                                    Block_Address_Type
                                      (Rose.System_Calls.Get_Word_64
                                         (Params, 0));
                  Block_Count   : constant Natural :=
                                    Natural
                                      (Rose.System_Calls.Get_Word_64
                                         (Params, 2));
               begin
                  ATA.Commands.Read_Sectors
                    (Drive         =>
                       ATA.Drives.Get
                         (ATA.Drives.ATA_Drive_Index (Params.Identifier)),
                     Address       => Block_Address,
                     Count         => Block_Count,
                     Sectors       => Buffer);
               end;

            when Rose.Interfaces.Block_Device.Write_Blocks_Endpoint =>
               declare
                  use Rose.Interfaces.Block_Device;
                  use System.Storage_Elements;
                  Buffer        : Storage_Array (1 .. Params.Buffer_Length);
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Params.Buffer_Address;
                  Block_Address : constant Block_Address_Type :=
                                    Block_Address_Type
                                      (Rose.System_Calls.Get_Word_64
                                         (Params, 0));
                  Block_Count   : constant Natural :=
                                    Natural
                                      (Rose.System_Calls.Get_Word_64
                                         (Params, 2));
               begin
                  ATA.Commands.Write_Sectors
                    (Drive         =>
                       ATA.Drives.Get
                         (ATA.Drives.ATA_Drive_Index (Params.Identifier)),
                     Address       => Block_Address,
                     Count         => Block_Count,
                     Sectors       => Buffer);
               end;

            when Rose.Interfaces.Ata.Get_Device_Endpoint =>
               declare
                  Get_Parameters_Cap : Rose.Capabilities.Capability;
                  Read_Block_Cap     : Rose.Capabilities.Capability;
                  Write_Block_Cap    : Rose.Capabilities.Capability;
               begin
                  Get_Interface (Params.Data (0),
                                 Get_Parameters_Cap,
                                 Read_Block_Cap,
                                 Write_Block_Cap);
                  Rose.System_Calls.Send_Cap (Reply, Get_Parameters_Cap);
                  Rose.System_Calls.Send_Cap (Reply, Read_Block_Cap);
                  Rose.System_Calls.Send_Cap (Reply, Write_Block_Cap);
               end;

            when others =>
               Rose.Console_IO.Put
                 ("ata: unknown endpoint: ");
               Rose.Console_IO.Put (Rose.Words.Word_64 (Params.Endpoint));
               Rose.Console_IO.New_Line;
               Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         end case;

         Rose.System_Calls.Invoke_Capability (Reply);

      end loop;
   end Start_Server;

end ATA.Server;
