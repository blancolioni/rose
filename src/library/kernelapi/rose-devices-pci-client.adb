with Rose.System_Calls.Client;

package body Rose.Devices.PCI.Client is

   Last_Get_Cap  : Rose.Capabilities.Capability :=
                     Rose.Capabilities.Null_Capability;
   Last_Get_Reg  : Register_32 := 0;
   Last_Get_Data : Rose.Words.Word_32 := 0;

   ---------
   -- Get --
   ---------

   function Get
     (Device   : Rose.Capabilities.Capability;
      Register : Register_8)
      return Rose.Words.Word_8
   is
      use Rose.Words;
      R_32  : constant Register_32 := Register_32 (Register / 4);
      Value : constant Rose.Words.Word_32 := Get (Device, R_32);
   begin
      return Word_8 (Value / 2 ** (Natural (Register) mod 4 * 8) mod 2 ** 8);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Device   : Rose.Capabilities.Capability;
      Register : Register_16)
      return Rose.Words.Word_16
   is
      use Rose.Words;
      R_32  : constant Register_32 := Register_32 (Register / 2);
      Value : constant Rose.Words.Word_32 := Get (Device, R_32);
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
     (Device   : Rose.Capabilities.Capability;
      Register : Register_32)
      return Rose.Words.Word_32
   is
      use type Rose.Capabilities.Capability;
   begin
      if Last_Get_Cap /= Device
        or else Last_Get_Reg /= Register
      then
         Last_Get_Data :=
           Rose.System_Calls.Client.Get_Value
             (Device, Rose.Words.Word (Register));
         Last_Get_Cap := Device;
         Last_Get_Reg := Register;
      end if;

      return Last_Get_Data;
   end Get;

end Rose.Devices.PCI.Client;
