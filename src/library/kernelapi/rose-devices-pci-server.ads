with Rose.Capabilities;

package Rose.Devices.PCI.Server is

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

end Rose.Devices.PCI.Server;
