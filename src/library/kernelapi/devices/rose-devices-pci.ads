with Rose.Words;

package Rose.Devices.PCI is

   pragma Pure (Rose.Devices.PCI);

   use type Rose.Words.Word_32;


   type Bus_Type      is mod 2 ** 8;
   type Device_Type   is mod 2 ** 5;
   type Function_Type is mod 2 ** 3;

   type Device_Address is new Rose.Words.Word_32;

   function To_Address
     (Bus        : Rose.Devices.PCI.Bus_Type;
      Slot       : Rose.Devices.PCI.Device_Type;
      Fn         : Rose.Devices.PCI.Function_Type)
      return Device_Address
   is (2 ** 31
       + Device_Address (Bus) * 2 ** 16
       + Device_Address (Slot) * 2 ** 11
       + Device_Address (Fn) * 2 ** 8);

   type Register_32 is mod 2 ** 6;
   type Register_16 is mod 2 ** 7;
   type Register_8 is mod 2 ** 8;

   R_Vendor_Id : constant Register_16 := 0;
   R_Device_Id : constant Register_16 := 1;
   R_Command   : constant Register_16 := 2;
   R_Status    : constant Register_16 := 3;

   R_Revision  : constant Register_8 := 16#08#;
   R_Prog_IF   : constant Register_8 := 16#09#;
   R_Sub_Class : constant Register_8 := 16#0A#;
   R_Class     : constant Register_8 := 16#0B#;

   R_Cache_Line_Size : constant Register_8 := 16#0C#;
   R_Latency_Timer   : constant Register_8 := 16#0D#;
   R_Header_Type     : constant Register_8 := 16#0E#;
   R_Self_Test       : constant Register_8 := 16#0F#;

   R_Interrupt_Line  : constant Register_8 := 16#3C#;
   R_Interrupt_PIN   : constant Register_8 := 16#3D#;
   R_Min_Grant       : constant Register_8 := 16#3E#;
   R_Max_Latency     : constant Register_8 := 16#3F#;

   R_BAR_0 : constant Register_32 := 4;
   R_BAR_1 : constant Register_32 := 5;
   R_BAR_2 : constant Register_32 := 6;
   R_BAR_3 : constant Register_32 := 7;
   R_BAR_4 : constant Register_32 := 8;
   R_BAR_5 : constant Register_32 := 9;

   R_Command_Status : constant Register_32 := 1;
   --  The command register, and above it the status register, whose bits
   --  are cleared by writing ones.

   Command_IO_Space          : constant Rose.Words.Word_32 := 16#0001#;
   Command_Memory_Space      : constant Rose.Words.Word_32 := 16#0002#;
   Command_Bus_Master        : constant Rose.Words.Word_32 := 16#0004#;
   Command_Interrupt_Disable : constant Rose.Words.Word_32 := 16#0400#;
   Command_Writable          : constant Rose.Words.Word_32 := 16#0407#;
   --  The command bits that a driver may set (#292)

   Multi_Function : constant Rose.Words.Word_8 := 16#80#;
   --  The header type's bit for a device with more than one function

   PCI_Display : constant Rose.Words.Word_8 := 3;
   PCI_Bridge  : constant Rose.Words.Word_8 := 6;
   --  The class codes of a display controller and a bridge

   function Byte_Of
     (Value    : Rose.Words.Word_32;
      Register : Register_8)
      return Rose.Words.Word_8
   is (Rose.Words.Word_8
         (Value / 2 ** (Natural (Register mod 4) * 8) mod 2 ** 8));
   --  The byte Register of the 32-bit register Value, which is the
   --  register Register / 4.

   PCI_Bar_IO_Mask     : constant Rose.Words.Word_32 := 16#FFFF_FFFC#;
   PCI_Bar_Memory_Mask : constant Rose.Words.Word_32 := 16#FFFF_FFF0#;

   PCI_Mass_Storage         : constant Rose.Words.Word_8 := 1;
   PCI_IDE                  : constant Rose.Words.Word_8 := 1;
   PCI_IDE_Primary_Native   : constant Rose.Words.Word_8 := 1;
   PCI_IDE_Secondary_Native : constant Rose.Words.Word_8 := 4;

   Invalid_Vendor_Id : constant Rose.Words.Word_16 := 16#FFFF#;
   Invalid_Device_Id : constant Rose.Words.Word_16 := 16#FFFF#;

end Rose.Devices.PCI;
