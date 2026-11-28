with Rose.Console_IO;
with Rose.Objects;
with Rose.Words;

with Rose.Devices.PCI.Server;

with Rose.Interfaces.Devices.Pci_Bus.Server;
with Rose.Interfaces.Devices.Pci_Device.Server;
with Rose.Server;
with Rose.System_Calls.Client;

with PCI.Devices;

package body PCI.Server is

   package Config renames Rose.Devices.PCI;

   use type Rose.Words.Word_8;
   use type Rose.Words.Word_32;
   use type PCI.Devices.BAR_Index;
   use type Rose.Devices.PCI.Register_32;
   use type PCI.Devices.Device_Count;

   Server : Rose.Server.Server_Context;

   function Read
     (Bus      : Config.Bus_Type;
      Slot     : Config.Device_Type;
      Fn       : Config.Function_Type;
      Register : Config.Register_32)
      return Rose.Words.Word_32
   is (Config.Server.Get
         (Command_Port_Out, Data_Port_In, Bus, Slot, Fn, Register));

   procedure Write
     (Bus      : Config.Bus_Type;
      Slot     : Config.Device_Type;
      Fn       : Config.Function_Type;
      Register : Config.Register_32;
      Value    : Rose.Words.Word_32);

   procedure Decode_BARs
     (Bus  : Config.Bus_Type;
      Slot : Config.Device_Type;
      Fn   : Config.Function_Type;
      BARs : out PCI.Devices.BAR_Array);
   --  Decode the six base address registers of a type 0 header, sizing
   --  each by writing all ones to it with the device's decoding switched
   --  off, and putting back what was there.

   procedure Add_Function
     (Bus  : Config.Bus_Type;
      Slot : Config.Device_Type;
      Fn   : Config.Function_Type);
   --  Add the function at Bus, Slot and Fn, if there is one.

   --  The handlers

   function Find_Device
     (Id     : Rose.Objects.Capability_Identifier;
      Vendor : Rose.Words.Word_32;
      Device : Rose.Words.Word_32;
      Index  : Natural)
      return Rose.Capabilities.Capability;

   function Owns (Id : Rose.Objects.Capability_Identifier) return Boolean
   is (PCI.Devices.Find (Id) /= 0);

   function Device_Of
     (Id : Rose.Objects.Capability_Identifier)
      return PCI.Devices.Device_Record
   is (PCI.Devices.Get (PCI.Devices.Find (Id)));
   --  Only called for an identifier that Owns accepts, since the generated
   --  server refuses the others.

   function Vendor_Id
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_32
   is (Device_Of (Id).Vendor);

   function Device_Id
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_32
   is (Device_Of (Id).Device);

   function Read_Config
     (Id       : Rose.Objects.Capability_Identifier;
      Register : Natural)
      return Rose.Words.Word_32;

   procedure Set_Command
     (Id   : Rose.Objects.Capability_Identifier;
      Bits : Rose.Words.Word_32);

   procedure Get_BAR
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Natural;
      Is_IO : out Boolean;
      Base  : out Rose.Words.Word_32;
      Size  : out Rose.Words.Word_32);

   function Interrupt_Line
     (Id : Rose.Objects.Capability_Identifier)
      return Natural
   is (Device_Of (Id).Interrupt_Line);

   ------------------
   -- Add_Function --
   ------------------

   procedure Add_Function
     (Bus  : Config.Bus_Type;
      Slot : Config.Device_Type;
      Fn   : Config.Function_Type)
   is
      Ids : constant Rose.Words.Word_32 := Read (Bus, Slot, Fn, 0);
   begin
      if Ids mod 65536 = Rose.Words.Word_32 (Config.Invalid_Vendor_Id) then
         return;
      end if;

      declare
         Header    : constant Rose.Words.Word_8 :=
                       Config.Byte_Of
                         (Read (Bus, Slot, Fn, 3), Config.R_Header_Type)
                       mod 128;
         Class     : constant Rose.Words.Word_8 :=
                       Config.Byte_Of
                         (Read (Bus, Slot, Fn, 2), Config.R_Class);
         Interrupt : constant Rose.Words.Word_32 := Read (Bus, Slot, Fn, 15);
         Line      : constant Rose.Words.Word_8 :=
                       Config.Byte_Of (Interrupt, Config.R_Interrupt_Line);
         Pin       : constant Rose.Words.Word_8 :=
                       Config.Byte_Of (Interrupt, Config.R_Interrupt_PIN);
         Item      : PCI.Devices.Device_Record :=
                       (Bus            => Bus,
                        Slot           => Slot,
                        Fn             => Fn,
                        Vendor         => Ids mod 65536,
                        Device         => Ids / 65536,
                        BARs           => (others => <>),
                        Interrupt_Line =>
                          (if Pin = 0 or else Line > 15
                           then PCI.Devices.No_Interrupt
                           else Natural (Line)));
      begin

         --  Only a type 0 header has six BARs. A display's BARs are
         --  left alone, since switching off its decoding while sizing
         --  them would hide the boot console's screen, and so are a
         --  bridge's, since a chipset can use those registers for other
         --  things (#292).

         if Header = 0
           and then Class /= Config.PCI_Display
           and then Class /= Config.PCI_Bridge
         then
            Decode_BARs (Bus, Slot, Fn, Item.BARs);
         end if;

         PCI.Devices.Add_Device (Item);
      end;
   end Add_Function;

   -----------------
   -- Create_Caps --
   -----------------

   procedure Create_Caps is
      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));
   begin
      Console_Cap := Get_Cap (1);
      Rose.Console_IO.Open (Console_Cap);
      Rose.Console_IO.Put_Line ("pci: starting");

      Command_Port_Out := Get_Cap (2);
      Data_Port_Out := Get_Cap (3);
      Data_Port_In := Get_Cap (4);

      --  Nothing reads the cap set again (#826).

      Rose.System_Calls.Delete_Cap (Get_Cap_From_Set);
   end Create_Caps;

   -----------------
   -- Decode_BARs --
   -----------------

   procedure Decode_BARs
     (Bus  : Config.Bus_Type;
      Slot : Config.Device_Type;
      Fn   : Config.Function_Type;
      BARs : out PCI.Devices.BAR_Array)
   is
      Command : constant Rose.Words.Word_32 :=
                  Read (Bus, Slot, Fn, Config.R_Command_Status) mod 65536;
      --  The status half isn't written back: its bits are cleared by
      --  writing ones.
      Upper   : Boolean := False;
   begin
      BARs := (others => <>);

      Write (Bus, Slot, Fn, Config.R_Command_Status,
             Command and not (Config.Command_IO_Space
                              or Config.Command_Memory_Space));

      for Index in PCI.Devices.BAR_Index loop
         declare
            BAR      : PCI.Devices.BAR_Record renames BARs (Index);
            Register : constant Config.Register_32 :=
                         Config.R_BAR_0 + Config.Register_32 (Index);
            Original : constant Rose.Words.Word_32 :=
                         Read (Bus, Slot, Fn, Register);
            Sized    : Rose.Words.Word_32;
         begin
            if Upper then
               BAR.Upper_Half := True;
               if Original /= 0 then
                  BARs (Index - 1).Size := 0;
               end if;
               Upper := False;
            else
               Write (Bus, Slot, Fn, Register, Rose.Words.Word_32'Last);
               Sized := Read (Bus, Slot, Fn, Register);
               Write (Bus, Slot, Fn, Register, Original);

               if Original mod 2 = 1 then
                  BAR.Is_IO := True;
                  BAR.Base := Original and Config.PCI_Bar_IO_Mask;
                  Sized := Sized and Config.PCI_Bar_IO_Mask;
                  if Sized /= 0 and then Sized < 65536 then
                     Sized := Sized or 16#FFFF_0000#;
                  end if;
               else
                  BAR.Base := Original and Config.PCI_Bar_Memory_Mask;
                  Sized := Sized and Config.PCI_Bar_Memory_Mask;
                  Upper := Original / 2 mod 4 = 2;
               end if;

               BAR.Size := (if Sized = 0 then 0 else (not Sized) + 1);
            end if;
         end;
      end loop;

      Write (Bus, Slot, Fn, Config.R_Command_Status, Command);
   end Decode_BARs;

   -----------------
   -- Find_Device --
   -----------------

   function Find_Device
     (Id     : Rose.Objects.Capability_Identifier;
      Vendor : Rose.Words.Word_32;
      Device : Rose.Words.Word_32;
      Index  : Natural)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      Found : constant PCI.Devices.Device_Count :=
                PCI.Devices.Find (Vendor, Device, Index);
   begin
      if Found = 0 then
         Rose.Server.Fail;
         return Rose.Capabilities.Null_Capability;
      end if;

      return Rose.Server.Create_Endpoint
        (Rose.Interfaces.Devices.Pci_Device.Pci_Device_Interface,
         PCI.Devices.Identifier (PCI.Devices.Get (Found)));
   end Find_Device;

   -------------
   -- Get_BAR --
   -------------

   procedure Get_BAR
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Natural;
      Is_IO : out Boolean;
      Base  : out Rose.Words.Word_32;
      Size  : out Rose.Words.Word_32)
   is
   begin
      Is_IO := False;
      Base := 0;
      Size := 0;

      if Index > 5 then
         Rose.Server.Fail;
         return;
      end if;

      declare
         BAR : constant PCI.Devices.BAR_Record :=
                 Device_Of (Id).BARs (PCI.Devices.BAR_Index (Index));
      begin
         if BAR.Upper_Half then
            Rose.Server.Fail;
         else
            Is_IO := BAR.Is_IO;
            Base := BAR.Base;
            Size := BAR.Size;
         end if;
      end;
   end Get_BAR;

   -----------------
   -- Read_Config --
   -----------------

   function Read_Config
     (Id       : Rose.Objects.Capability_Identifier;
      Register : Natural)
      return Rose.Words.Word_32
   is
   begin
      if Register > 63 then
         Rose.Server.Fail;
         return 0;
      end if;

      declare
         Item : constant PCI.Devices.Device_Record := Device_Of (Id);
      begin
         return Read (Item.Bus, Item.Slot, Item.Fn,
                      Config.Register_32 (Register));
      end;
   end Read_Config;

   ------------------
   -- Scan_Devices --
   ------------------

   procedure Scan_Devices is
   begin
      for Bus in Config.Bus_Type loop
         for Slot in Config.Device_Type loop
            declare
               Ids : constant Rose.Words.Word_32 := Read (Bus, Slot, 0, 0);
            begin
               if Ids mod 65536
                 /= Rose.Words.Word_32 (Config.Invalid_Vendor_Id)
               then
                  if Config.Byte_Of
                    (Read (Bus, Slot, 0, 3), Config.R_Header_Type)
                    >= Config.Multi_Function
                  then
                     for Fn in Config.Function_Type loop
                        Add_Function (Bus, Slot, Fn);
                     end loop;
                  else
                     Add_Function (Bus, Slot, 0);
                  end if;
               end if;
            end;
         end loop;
      end loop;
   end Scan_Devices;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command
     (Id   : Rose.Objects.Capability_Identifier;
      Bits : Rose.Words.Word_32)
   is
   begin
      if (Bits and not Config.Command_Writable) /= 0 then
         Rose.Server.Fail;
         return;
      end if;

      declare
         Item : constant PCI.Devices.Device_Record := Device_Of (Id);
         Old  : constant Rose.Words.Word_32 :=
                  Read (Item.Bus, Item.Slot, Item.Fn,
                        Config.R_Command_Status) mod 65536;
      begin
         Write (Item.Bus, Item.Slot, Item.Fn, Config.R_Command_Status,
                (Old and not Config.Command_Writable) or Bits);
      end;
   end Set_Command;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Interfaces.Devices.Pci_Bus.Server.Create_Server
        (Server, Find_Device'Access);
      Rose.Interfaces.Devices.Pci_Device.Server.Attach_Interface
        (Server_Context => Server,
         Vendor_Id      => Vendor_Id'Access,
         Device_Id      => Device_Id'Access,
         Read_Config    => Read_Config'Access,
         Set_Command    => Set_Command'Access,
         Get_Bar        => Get_BAR'Access,
         Interrupt_Line => Interrupt_Line'Access,
         Instanced      => True);
      Rose.Interfaces.Devices.Pci_Device.Server.Set_Owns (Owns'Access);
      Rose.Server.Start_Server (Server);
   end Start_Server;

   -----------
   -- Write --
   -----------

   procedure Write
     (Bus      : Config.Bus_Type;
      Slot     : Config.Device_Type;
      Fn       : Config.Function_Type;
      Register : Config.Register_32;
      Value    : Rose.Words.Word_32)
   is
   begin
      Config.Server.Put
        (Command_Port_Out, Data_Port_Out, Bus, Slot, Fn, Register, Value);
   end Write;

end PCI.Server;
