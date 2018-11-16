with Rose.Objects;
with Rose.Words;

with Rose.Devices.PCI;

package PCI.Devices is

   Max_PCI_Devices : constant := 20;

   procedure Add_Device
     (Bus        : Rose.Devices.PCI.Bus_Type;
      Slot       : Rose.Devices.PCI.Device_Type;
      Fn         : Rose.Devices.PCI.Function_Type;
      Vendor     : Rose.Words.Word_16;
      Device     : Rose.Words.Word_16;
      Device_EP  : Rose.Objects.Endpoint_Id;
      Device_Cap : Rose.Capabilities.Capability);

   function Find_Device_Cap
     (Vendor, Device : Rose.Words.Word_16)
      return Rose.Capabilities.Capability;

   procedure Get_Device
     (Device_EP  : Rose.Objects.Endpoint_Id;
      Found      : out Boolean;
      Bus        : out Rose.Devices.PCI.Bus_Type;
      Slot       : out Rose.Devices.PCI.Device_Type;
      Fn         : out Rose.Devices.PCI.Function_Type);

private

   type PCI_Device_Record is
      record
         Active  : Boolean := False;
         Address : Rose.Devices.PCI.Device_Address;
         Bus     : Rose.Devices.PCI.Bus_Type;
         Slot    : Rose.Devices.PCI.Device_Type;
         Fn      : Rose.Devices.PCI.Function_Type;
         Vendor  : Rose.Words.Word_16;
         Device  : Rose.Words.Word_16;
         EP      : Rose.Objects.Endpoint_Id;
         Cap     : Rose.Capabilities.Capability;
      end record;

   PCI_Device_Count : Natural := 0;

   type PCI_Device_Table is array (1 .. Max_PCI_Devices) of PCI_Device_Record;

end PCI.Devices;
