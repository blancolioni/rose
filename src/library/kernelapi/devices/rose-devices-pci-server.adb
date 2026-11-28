with Rose.Devices.Port_IO;

package body Rose.Devices.PCI.Server is

   function Register_Address
     (Bus      : Bus_Type;
      Device   : Device_Type;
      Fn       : Function_Type;
      Register : Register_32)
      return Rose.Words.Word_32
   is (Rose.Words.Word_32 (To_Address (Bus, Device, Fn))
       + Rose.Words.Word_32 (Register) * 4);

   ---------
   -- Get --
   ---------

   function Get
     (Command_Out_Port : Rose.Capabilities.Capability;
      Data_In_Port     : Rose.Capabilities.Capability;
      Bus              : Bus_Type;
      Device           : Device_Type;
      Fn               : Function_Type;
      Register         : Register_8)
      return Rose.Words.Word_8
   is
   begin
      return Byte_Of
        (Get (Command_Out_Port, Data_In_Port,
              Bus, Device, Fn, Register_32 (Register / 4)),
         Register);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Command_Out_Port : Rose.Capabilities.Capability;
      Data_In_Port     : Rose.Capabilities.Capability;
      Bus              : Bus_Type;
      Device           : Device_Type;
      Fn               : Function_Type;
      Register         : Register_16)
      return Rose.Words.Word_16
   is
      use Rose.Words;
      Value : constant Rose.Words.Word_32 :=
                Get (Command_Out_Port, Data_In_Port,
                     Bus, Device, Fn, Register_32 (Register / 2));
   begin
      if Register mod 2 = 0 then
         return Word_16 (Value mod 65536);
      else
         return Word_16 (Value / 65536);
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Command_Out_Port : Rose.Capabilities.Capability;
      Data_In_Port     : Rose.Capabilities.Capability;
      Bus              : Bus_Type;
      Device           : Device_Type;
      Fn               : Function_Type;
      Register         : Register_32)
      return Rose.Words.Word_32
   is
   begin
      Rose.Devices.Port_IO.Port_Out_32
        (Command_Out_Port, Register_Address (Bus, Device, Fn, Register));
      return Rose.Devices.Port_IO.Port_In_32 (Data_In_Port);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (Command_Out_Port : Rose.Capabilities.Capability;
      Data_Out_Port    : Rose.Capabilities.Capability;
      Bus              : Bus_Type;
      Device           : Device_Type;
      Fn               : Function_Type;
      Register         : Register_32;
      Value            : Rose.Words.Word_32)
   is
   begin
      Rose.Devices.Port_IO.Port_Out_32
        (Command_Out_Port, Register_Address (Bus, Device, Fn, Register));
      Rose.Devices.Port_IO.Port_Out_32 (Data_Out_Port, Value);
   end Put;

end Rose.Devices.PCI.Server;
