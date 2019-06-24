with Rose.Capabilities;

package PCI is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Take_Next_Cap       : constant Rose.Capabilities.Capability := 2;

   Console_Cap         : Rose.Capabilities.Capability;
   Command_Port_Out    : Rose.Capabilities.Capability;
   Data_Port_Out       : Rose.Capabilities.Capability;
   Data_Port_In        : Rose.Capabilities.Capability;

   type Vendor_Id is mod 2 ** 16;
   Invalid_Vendor_Id : constant Vendor_Id := 16#FFFF#;

   type Device_Id is mod 2 ** 16;
   Invalid_Device_Id : constant Device_Id := 16#FFFF#;

end PCI;
