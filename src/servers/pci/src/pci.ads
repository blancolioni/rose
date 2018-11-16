with Rose.Capabilities;

package PCI is

   pragma Pure (PCI);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;
   Command_Port_Out    : constant Rose.Capabilities.Capability := 3;
   Data_Port_Out       : constant Rose.Capabilities.Capability := 4;
   Data_Port_In        : constant Rose.Capabilities.Capability := 5;

   type Vendor_Id is mod 2 ** 16;
   Invalid_Vendor_Id : constant Vendor_Id := 16#FFFF#;

   type Device_Id is mod 2 ** 16;
   Invalid_Device_Id : constant Device_Id := 16#FFFF#;

end PCI;
