with System.Storage_Elements;

with Rose.System_Calls.Client;
with Rose.Words;                       use Rose.Words;

with Rose.Devices.PCI.Client;
with Rose.Console_IO;

with Rose.Devices.Block.Server;

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

   procedure Read_Block
     (Block_Address : Rose.Devices.Block.Block_Address_Type;
      Buffer        : out System.Storage_Elements.Storage_Array);

   procedure Write_Block
     (Block_Address : Rose.Devices.Block.Block_Address_Type;
      Buffer        : System.Storage_Elements.Storage_Array);

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

   ----------------
   -- Read_Block --
   ----------------

   procedure Read_Block
     (Block_Address : Rose.Devices.Block.Block_Address_Type;
      Buffer        : out System.Storage_Elements.Storage_Array)
   is
   begin
      ATA.Drives.Read_Block (0, Block_Address, Buffer);
   end Read_Block;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Drive : constant ATA.Drives.ATA_Drive :=
                ATA.Drives.Get (0);
   begin
      Rose.Devices.Block.Server.Run_Server
        (Endpoint_Cap  => Create_Endpoint_Cap,
         Block_Size    => ATA.Drives.Block_Size (Drive),
         Block_Count   => ATA.Drives.Block_Count (Drive),
         Read_Handler  => Read_Block'Access,
         Write_Handler => Write_Block'Access);
   end Start_Server;

   -----------------
   -- Write_Block --
   -----------------

   procedure Write_Block
     (Block_Address : Rose.Devices.Block.Block_Address_Type;
      Buffer        : System.Storage_Elements.Storage_Array)
   is
   begin
      ATA.Drives.Write_Block (0, Block_Address, Buffer);
   end Write_Block;

end ATA.Server;
