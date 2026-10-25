with System.Storage_Elements;

with Rose.Addresses;
with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Region.Client;

package Mem.Processes is

   function Is_Valid_Process_Id
     (Process : Process_Id)
      return Boolean;

   function Get_Object_Id
     (Process : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id;

   function Get_Process_Id
     (Process : Rose.Objects.Object_Id)
      return Process_Id;

   function Get_Process_Heap_Bound
     (Process : Process_Id)
      return Rose.Addresses.Virtual_Page_Address;

   function Get_Page_Object_Id
     (Process         : Process_Id;
      Virtual_Page    : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Object_Id;

   function Get_Virtual_Address
     (Process : Process_Id;
      Page    : Rose.Objects.Page_Object_Id)
     return Rose.Addresses.Virtual_Page_Address;

   procedure Get_Process_Segment
     (Process         : Process_Id;
      Virtual_Page    : Rose.Addresses.Virtual_Page_Address;
      Page_Object     : out Rose.Objects.Object_Id;
      Valid           : out Boolean;
      Readable        : out Boolean;
      Writable        : out Boolean;
      Executable      : out Boolean;
      Persistent      : out Boolean);

   procedure Resume_Process
     (Process         : Process_Id);

   procedure Kill_Process
     (Process         : Process_Id);

   procedure Fault_Process
     (Process : Process_Id;
      Message : String);

   function New_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   function Register_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Process_Id;

   function Get_Heap_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability;

   procedure Set_Published_Interface_Cap
     (Process : Process_Id;
      Cap     : Rose.Capabilities.Capability);

   function Published_Interface_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability;

   procedure Initialize_Page
     (Process       : Process_Id;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address);

   procedure Add_Segment
     (Process       : Process_Id;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound : Rose.Addresses.Virtual_Page_Address;
      Region        : Rose.Interfaces.Region.Client.Region_Client;
      Region_Offset : Rose.Words.Word;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Resizable     : Boolean;
      Private_Region : Boolean := False);
   --  Private_Region is True if Region belongs to the process, and is to
   --  be released when the process is gone.

   type Fault_Kind is (Region_Fault, Zero_Fault, Write_Fault, Invalid_Fault);
   --  A page read from its segment's region, a page filled with zeroes,
   --  a write to a mapped page, which makes it writable and dirty, and an
   --  access that is not allowed.

   procedure Count_Fault
     (Process : Process_Id;
      Kind    : Fault_Kind;
      Ticks   : Natural);
   --  Count a fault of Process that took Ticks to handle. The counts are
   --  reported as trace events when the process is gone (#195).

   procedure Add_Nonpersistent_Segment
     (Process       : Process_Id;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound : Rose.Addresses.Virtual_Page_Address;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Resizable     : Boolean);

   procedure Add_Environment
     (Process       : Process_Id;
      Environment   : System.Storage_Elements.Storage_Array);

   procedure Resize_Segment
     (Process           : Process_Id;
      New_Virtual_Bound : Rose.Addresses.Virtual_Page_Address);

   procedure Allocate_Physical_Page
     (Process       : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      R, W, X, P    : Boolean := False);

end Mem.Processes;
