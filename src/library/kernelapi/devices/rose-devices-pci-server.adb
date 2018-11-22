with Rose.Devices.Port_IO;

package body Rose.Devices.PCI.Server is

   Last_Get_Address  : Rose.Words.Word_32 := Rose.Words.Word_32'Last;
   Last_Get_Data     : Rose.Words.Word_32 := 0;

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
      use Rose.Words;
      R_32  : constant Register_32 := Register_32 (Register / 4);
      Value : constant Rose.Words.Word_32 :=
                Get (Command_Out_Port, Data_In_Port,
                     Bus, Device, Fn, R_32);
   begin
      return Word_8 (Value / 2 ** (Natural (Register) mod 4 * 8) mod 2 ** 8);
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
      R_32  : constant Register_32 := Register_32 (Register / 2);
      Value : constant Rose.Words.Word_32 :=
                Get (Command_Out_Port, Data_In_Port,
                     Bus, Device, Fn, R_32);
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
      use Rose.Words;
      Address : constant Rose.Words.Word :=
                  Word_32 (To_Address (Bus, Device, Fn))
                  + Word_32 (Register) * 4;
   begin
      if Last_Get_Address /= Address then
         Rose.Devices.Port_IO.Port_Out_32 (Command_Out_Port, Address);
         Last_Get_Data := Rose.Devices.Port_IO.Port_In_32 (Data_In_Port);
         Last_Get_Address := Address;
      end if;
      return Last_Get_Data;
   end Get;

end Rose.Devices.PCI.Server;
