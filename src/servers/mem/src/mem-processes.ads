with Rose.Addresses;
with Rose.Capabilities;
with Rose.Objects;

with Rose.Interfaces.Region.Client;

package Mem.Processes is

   function Is_Valid_Process_Id
     (Process : Rose.Objects.Capability_Identifier)
      return Boolean;

   function Get_Object_Id
     (Process : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id;

   function Get_Process_Id
     (Process : Rose.Objects.Object_Id)
      return Rose.Objects.Capability_Identifier;

   procedure Get_Process_Segment
     (Process         : Rose.Objects.Capability_Identifier;
      Virtual_Page    : Rose.Addresses.Virtual_Page_Address;
      Page_Object     : out Rose.Objects.Object_Id;
      Valid           : out Boolean;
      Readable        : out Boolean;
      Writable        : out Boolean;
      Executable      : out Boolean);

   procedure Resume_Process
     (Process         : Rose.Objects.Capability_Identifier);

   procedure Kill_Process
     (Process         : Rose.Objects.Capability_Identifier);

   procedure Fault_Process
     (Process         : Rose.Objects.Capability_Identifier);

   function New_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   function Register_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Objects.Capability_Identifier;

   procedure Initialize_Page
     (Process       : Rose.Objects.Capability_Identifier;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address);

   procedure Add_Segment
     (Process       : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Region        : Rose.Interfaces.Region.Client.Region_Client;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean);

   procedure Add_Nonpersistent_Segment
     (Process       : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound : Rose.Addresses.Virtual_Page_Address;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean);

end Mem.Processes;
