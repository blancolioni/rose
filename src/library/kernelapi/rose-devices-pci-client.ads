with Rose.Capabilities;

package Rose.Devices.PCI.Client is

   function Get
     (Device   : Rose.Capabilities.Capability;
      Register : Register_8)
      return Rose.Words.Word_8;

   function Get
     (Device   : Rose.Capabilities.Capability;
      Register : Register_16)
      return Rose.Words.Word_16;

   function Get
     (Device   : Rose.Capabilities.Capability;
      Register : Register_32)
      return Rose.Words.Word_32;

end Rose.Devices.PCI.Client;
