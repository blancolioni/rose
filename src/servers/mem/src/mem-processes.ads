with Rose.Addresses;
with Rose.Capabilities;
with Rose.Objects;

private with Rose.Limits;

package Mem.Processes is

   type Process_State is (Available, Active);

   type Segment_Flag is (Read, Write, Execute);

   type Segment_Flag_Array is array (Segment_Flag) of Boolean;

   type Segment_Index is range 1 .. 4;

   type Segment_Record is
      record
         Base    : Rose.Addresses.Virtual_Page_Address := 0;
         Bound   : Rose.Addresses.Virtual_Page_Address := 0;
         Flags   : Segment_Flag_Array := (others => False);
      end record;

   type Segment_Record_Array is array (Segment_Index) of Segment_Record;

   procedure New_Process
     (Process     : Rose.Objects.Process_Id;
      Resume_Cap  : Rose.Capabilities.Capability;
      Faulted_Cap : Rose.Capabilities.Capability;
      Segments    : Segment_Record_Array);

   procedure Kill_Process (Process : Rose.Objects.Process_Id);

   function Resume_Capability
     (Process : Rose.Objects.Process_Id)
      return Rose.Capabilities.Capability;

   function Faulted_Capability
     (Process : Rose.Objects.Process_Id)
      return Rose.Capabilities.Capability;

   function Is_Valid_Process_Id
     (Process : Rose.Objects.Process_Id)
      return Boolean;

   procedure Get_Process_Segment
     (Process         : Rose.Objects.Process_Id;
      Virtual_Address : Rose.Addresses.Virtual_Page_Address;
      Valid           : out Boolean;
      Readable        : out Boolean;
      Writable        : out Boolean;
      Executable      : out Boolean);

private

   type Memory_Process_Record is
      record
         State       : Process_State := Available;
         Resume_Cap  : Rose.Capabilities.Capability := 0;
         Faulted_Cap : Rose.Capabilities.Capability := 0;
         Segments    : Segment_Record_Array;
      end record;

   type Memory_Process_Table is
     array (Rose.Objects.Process_Id range 1 .. Rose.Limits.Max_Processes)
     of aliased Memory_Process_Record;

   Process_Table : Memory_Process_Table;

   function Resume_Capability
     (Process : Rose.Objects.Process_Id)
      return Rose.Capabilities.Capability
   is (Process_Table (Process).Resume_Cap);

   function Faulted_Capability
     (Process : Rose.Objects.Process_Id)
      return Rose.Capabilities.Capability
   is (Process_Table (Process).Faulted_Cap);

   function Is_Valid_Process_Id
     (Process : Rose.Objects.Process_Id)
      return Boolean
   is (Process in Process_Table'Range
       and then Process_Table (Process).State = Active);

end Mem.Processes;
