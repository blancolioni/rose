with System.Storage_Elements;

with Rose.Addresses;
with Rose.Capabilities;
with Rose.Checkpoints;
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
   --  The object id of the process whose caps have the identifier
   --  Process: the Process interface's Get_Object_Id.

   function Process_Object_Id
     (Process : Process_Id)
      return Rose.Objects.Object_Id;
   --  The object id of the process in slot Process.

   function Find_Process
     (Identifier : Rose.Objects.Capability_Identifier)
      return Process_Id;
   --  The process whose caps have Identifier, or 0 if there is none.

   function Process_Identifier
     (Process : Process_Id)
      return Rose.Objects.Capability_Identifier;
   --  The identifier of the caps that mem makes for Process. It comes from
   --  the process's object id, which is never used again and is the same
   --  after a restore, and not from its slot, which the next process can
   --  take. So a cap for an ended process reaches no other process
   --  (#616).

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
     (Process_Cap : Rose.Capabilities.Capability;
      Bank_Cap    : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;
   --  Bank_Cap is the bank that the process is launched against: its own
   --  bank is a new sub-bank of it (ADR 0015, #750).  Mem deletes
   --  Bank_Cap.

   procedure Set_Process_Banks (Banks_Cap : Rose.Capabilities.Capability);
   --  The storage server's Process_Banks, which init gives mem at every
   --  boot (#750).

   procedure Set_Ended (Ended_Cap : Rose.Capabilities.Capability);
   --  Give the store the cap to send to when it queues processes to end.

   function Get_Bank_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability;
   --  The cap for Process's own bank, or the null cap.  Mem keeps one,
   --  and asks the store for it again after a restore.

   procedure End_Bank (Process : Rose.Objects.Object_Id);
   --  Process has ended: tell the store to destroy its own bank, which
   --  ends the processes whose banks are below it.

   function Find_By_Oid
     (Oid : Rose.Objects.Object_Id)
      return Process_Id;
   --  The process whose object id is Oid, or 0.

   function Take_Ended return Rose.Objects.Object_Id;
   --  The next process to end because its bank was destroyed, from the
   --  store's Process_Banks, or 0.

   function Register_Process
     (Process_Cap : Rose.Capabilities.Capability;
      Boot_Module : Boolean)
      return Process_Id;
   --  Give the kernel process Process_Cap a slot. Boot modules have slots
   --  of their own, below every launched process's, because the install
   --  media boots more of them than hd0 does: a slot a launched process
   --  had at a checkpoint must still be free when an update boot
   --  restores it (#873). 0 if there is no free slot.

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
      Private_Region : Boolean := False;
      Shared        : Boolean := False);
   --  Private_Region is True if Region belongs to the process, and is to
   --  be released when the process is gone. Shared is True if the segment
   --  maps the region's pages themselves, shared with every process that
   --  maps them shared (ADR 0025). A shared segment that overlaps another
   --  segment of the process is left out.

   procedure Remove_Segment
     (Process      : Process_Id;
      Virtual_Base : Rose.Addresses.Virtual_Page_Address);
   --  Unmap the segment that starts at Virtual_Base, and free its pages.
   --  The last mapping of a shared page writes it back to its region, if
   --  it has been changed.

   procedure Release_Shared_Segments (Process : Process_Id);
   --  Process is gone: drop its mappings of shared pages, and write back
   --  those that it was the last to map. Its other pages are freed by
   --  Mem.Page_Table.Delete_All.

   function Shared_Page
     (Process      : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Object_Id;
   --  The store page that Virtual_Page of Process maps, if it is in a
   --  shared segment, or zero.

   function Region_Page
     (Process      : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Object_Id;
   --  The store page that Virtual_Page of Process is read from, or zero if
   --  its segment has no region.

   type Fault_Kind is
     (Region_Fault, Zero_Fault, Cached_Fault, Write_Fault, Invalid_Fault);
   --  A page read from its segment's region, a page filled with zeroes,
   --  a page of a read-only segment that was already in memory (#200), a
   --  write to a mapped page, which makes it writable and dirty, and an
   --  access that is not allowed.

   procedure Count_Fault
     (Process : Process_Id;
      Kind    : Fault_Kind;
      Ticks   : Natural);
   --  Count a fault of Process that took Ticks to handle. The counts are
   --  reported as trace events when the process is gone (#195).

   function Store_Read_Count return Natural;
   --  The number of pages read from a region since boot, for the status
   --  bar (#210). Cache hits, zero fills and write faults are not counted.

   procedure Add_Nonpersistent_Segment
     (Process       : Process_Id;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound : Rose.Addresses.Virtual_Page_Address;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Resizable     : Boolean);

   procedure Resize_Segment
     (Process           : Process_Id;
      New_Virtual_Bound : Rose.Addresses.Virtual_Page_Address);

   procedure Allocate_Physical_Page
     (Process       : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      R, W, X, P    : Boolean := False);

   procedure Iterate_Saved_Processes
     (Process : not null access procedure (Pid : Process_Id));
   --  Call Process for each process that goes into a checkpoint: every
   --  process with a persistent segment, whether it is active or
   --  faulted.

   procedure Save_Process
     (Process : Process_Id;
      Item    : in out Rose.Checkpoints.Mem_Process_Record);
   --  Fill in Item, which is all zeroes, for Process.

   --  Restoring a checkpoint (the Memory_Restore interface).

   procedure Restore_Process
     (Id      : Rose.Objects.Capability_Identifier;
      Process : Rose.Capabilities.Capability;
      Mem_Pid : Natural);

   procedure Restore_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Mem_Pid       : Natural;
      Base          : Rose.Words.Word;
      Bound         : Rose.Words.Word;
      Limit         : Rose.Words.Word;
      Flags         : Rose.Words.Word;
      Region        : Rose.Capabilities.Capability;
      Region_Offset : Rose.Words.Word);

   procedure Restore_Zero_Segment
     (Id      : Rose.Objects.Capability_Identifier;
      Mem_Pid : Natural;
      Base    : Rose.Words.Word;
      Bound   : Rose.Words.Word;
      Limit   : Rose.Words.Word;
      Flags   : Rose.Words.Word);

   procedure Restore_Page
     (Id           : Rose.Objects.Capability_Identifier;
      Mem_Pid      : Natural;
      Virtual_Page : Rose.Words.Word;
      Page         : System.Storage_Elements.Storage_Array);

   procedure Restore_Shared_Page
     (Id         : Rose.Objects.Capability_Identifier;
      Store_Page : Rose.Objects.Object_Id;
      Page       : System.Storage_Elements.Storage_Array);

end Mem.Processes;
