with Rose.Capabilities;

package Rose.Devices.PCI.Server is

   --  Configuration space through the configuration address port (CF8)
   --  and the data port (CFC), for the pci boot server. Nothing is
   --  cached: a register can change between reads, and BAR sizing reads
   --  a register just after writing it.

   function Get
     (Command_Out_Port : Rose.Capabilities.Capability;
      Data_In_Port     : Rose.Capabilities.Capability;
      Bus              : Bus_Type;
      Device           : Device_Type;
      Fn               : Function_Type;
      Register         : Register_8)
      return Rose.Words.Word_8;

   function Get
     (Command_Out_Port : Rose.Capabilities.Capability;
      Data_In_Port     : Rose.Capabilities.Capability;
      Bus              : Bus_Type;
      Device           : Device_Type;
      Fn               : Function_Type;
      Register         : Register_16)
      return Rose.Words.Word_16;

   function Get
     (Command_Out_Port : Rose.Capabilities.Capability;
      Data_In_Port     : Rose.Capabilities.Capability;
      Bus              : Bus_Type;
      Device           : Device_Type;
      Fn               : Function_Type;
      Register         : Register_32)
      return Rose.Words.Word_32;

   procedure Put
     (Command_Out_Port : Rose.Capabilities.Capability;
      Data_Out_Port    : Rose.Capabilities.Capability;
      Bus              : Bus_Type;
      Device           : Device_Type;
      Fn               : Function_Type;
      Register         : Register_32;
      Value            : Rose.Words.Word_32);

end Rose.Devices.PCI.Server;
