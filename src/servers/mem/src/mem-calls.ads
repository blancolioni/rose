with Rose.Addresses;
with Rose.Capabilities;
with Rose.Objects;

with Mem.Processes;

package Mem.Calls is

   type Launch_Handler is access
     procedure (Process     : Rose.Objects.Object_Id;
                Resume_Cap  : Rose.Capabilities.Capability;
                Faulted_Cap : Rose.Capabilities.Capability;
                Segments    : Mem.Processes.Segment_Record_Array);

   type Kill_Handler is access
     procedure (Process : Rose.Objects.Object_Id);

   type Page_Fault_Handler is access
     procedure (Process       : Rose.Objects.Object_Id;
                Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
                Physical_Page : Rose.Addresses.Physical_Page_Address;
                Action        : Action_Type);

   procedure Receive
     (On_Launch     : Launch_Handler;
      On_Kill       : Kill_Handler;
      On_Page_Fault : Page_Fault_Handler);

   procedure Load_Memory_Map;

   procedure Map
     (Process    : Rose.Objects.Object_Id;
      Physical   : Rose.Addresses.Physical_Page_Address;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Readable   : Boolean;
      Writeable  : Boolean;
      Executable : Boolean);

   procedure Unmap
     (Process    : Rose.Objects.Object_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address);

   procedure Set_Ready
     (Process : Rose.Objects.Object_Id);

   procedure Set_Faulted
     (Process : Rose.Objects.Object_Id);

   procedure Set_Error
     (Process : Rose.Objects.Object_Id)
   is null;

   procedure Console_Message (Message : String)
   is null;

end Mem.Calls;
