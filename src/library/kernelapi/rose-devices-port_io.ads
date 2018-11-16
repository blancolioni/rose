with Rose.Capabilities;
with Rose.Words;

package Rose.Devices.Port_IO is

   function Port_In_8
     (Port   : Rose.Capabilities.Capability;
      Offset : Rose.Words.Word_8 := 0)
      return Rose.Words.Word_8;

   function Port_In_16
     (Port  : Rose.Capabilities.Capability)
      return Rose.Words.Word_16;

   function Port_In_32
     (Port  : Rose.Capabilities.Capability)
      return Rose.Words.Word_32;

   procedure Port_Out_8
     (Port  : Rose.Capabilities.Capability;
      Value : Rose.Words.Word_8);

   procedure Port_Out_16
     (Port  : Rose.Capabilities.Capability;
      Value : Rose.Words.Word_16);

   procedure Port_Out_32
     (Port  : Rose.Capabilities.Capability;
      Value : Rose.Words.Word_32);

   procedure Port_Out_8
     (Port    : Rose.Capabilities.Capability;
      Offset  : Rose.Words.Word_8;
      Value   : Rose.Words.Word_8);

   procedure Port_Out_16
     (Port    : Rose.Capabilities.Capability;
      Offset  : Rose.Words.Word_8;
      Value   : Rose.Words.Word_16);

   procedure Port_Out_32
     (Port    : Rose.Capabilities.Capability;
      Offset  : Rose.Words.Word_8;
      Value   : Rose.Words.Word_32);

   type Word_8_Data_Record is
      record
         Offset : Rose.Words.Word_8;
         Data   : Rose.Words.Word_8;
      end record;

   type Word_8_Data_Array is
     array (Positive range <>) of Word_8_Data_Record;

   procedure Port_Out_8
     (Port : Rose.Capabilities.Capability;
      Data : Word_8_Data_Array);

end Rose.Devices.Port_IO;
