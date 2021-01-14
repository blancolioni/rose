with System.Storage_Elements;

with Rose.Addresses;
with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;

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

   function Get_Process_Heap_Bound
     (Process : Rose.Objects.Capability_Identifier)
      return Rose.Addresses.Virtual_Page_Address;

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

   function Get_Heap_Cap
     (Process : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure Initialize_Page
     (Process       : Rose.Objects.Capability_Identifier;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address);

   procedure Add_Segment
     (Process       : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound : Rose.Addresses.Virtual_Page_Address;
      Region        : Rose.Interfaces.Region.Client.Region_Client;
      Region_Offset : Rose.Words.Word;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Resizable     : Boolean);

   procedure Add_Nonpersistent_Segment
     (Process       : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound : Rose.Addresses.Virtual_Page_Address;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Resizable     : Boolean);

   procedure Add_Environment
     (Process       : Rose.Objects.Capability_Identifier;
      Environment   : System.Storage_Elements.Storage_Array);

   procedure Resize_Segment
     (Process           : Rose.Objects.Capability_Identifier;
      New_Virtual_Bound : Rose.Addresses.Virtual_Page_Address);

   procedure Allocate_Physical_Page
     (Process       : Rose.Objects.Capability_Identifier;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      R, W, X       : Boolean := False);

end Mem.Processes;
