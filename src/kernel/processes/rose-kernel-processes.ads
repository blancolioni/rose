with Rose.Addresses;
with Rose.Capabilities.Layout;
with Rose.Invocation;
with Rose.Words;

private with Rose.Kernel.Arch;
private with Rose.Kernel.Cap_Tables;
private with Rose.Kernel.Derivations;
private with Rose.Kernel.Interrupts;
private with Rose.Kernel.Limits;

package Rose.Kernel.Processes is

   Process_Table_Heap_Size : constant Rose.Addresses.Physical_Bytes;
   Process_Priority_Count  : constant := 16;

   type Process_State is
     (Available, Starting, Ready,
      Running, Blocked, Interrupted, Faulted, Killed);

   type Process_Priority is range 1 .. Process_Priority_Count;

   type Process_Id is private;
   Null_Process_Id : constant Process_Id;

   function To_Process_Id
     (Oid : Rose.Objects.Object_Id)
      return Process_Id;
   --  The process whose object id is Oid, or Null_Process_Id if there is
   --  none: Oid is not a process's object id, or that process has gone.

   function To_Object_Id
     (Pid : Process_Id)
      return Rose.Objects.Object_Id;

   function New_Process
     (Name : String)
     return Process_Id;

   procedure Start_Process
     (Process : Process_Id;
      Address : Rose.Words.Word);

   procedure Kill_Process
     (Process : Process_Id);

   function Trace (Process : Process_Id) return Boolean;

   procedure Start_Trace (Process : Process_Id);
   procedure Stop_Trace (Process : Process_Id);

   procedure Set_Current_Invocation
     (Invocation : Rose.Invocation.Invocation_Record);

   procedure Get_Current_Invocation
     (Pid        : Process_Id;
      Invocation : out Rose.Invocation.Invocation_Record);

   procedure Get_Process_Name
     (Pid  : Process_Id;
      Name : out String;
      Last : out Natural);

   function Create_Cap
     (Pid : Process_Id)
      return Rose.Capabilities.Capability;

   procedure Create_Caps
     (Pid   : Process_Id;
      Caps  : out Rose.Capabilities.Capability_Array);
   --  Fill caps with new capabilities.  If any cannot be allocated,
   --  then none will be, and then first element of Caps will be zero.

   procedure Copy_Cap
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Cap             : Rose.Capabilities.Capability);

   function New_Cap
     (For_Process_Id : Process_Id;
      Layout         : Rose.Capabilities.Layout.Capability_Layout)
      return Rose.Capabilities.Capability;

   function Has_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Boolean;

   procedure Get_Cap
     (Pid    : Process_Id;
      Cap    : Rose.Capabilities.Capability;
      Layout : out Rose.Capabilities.Layout.Capability_Layout);

   function Cap_Type
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Type;

   procedure Rescind_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability);
   --  Make every copy of Cap that was not derived from it invalid.  If Cap
   --  was derived (Derive_Cap), only it and what was derived from it are
   --  made invalid, and Cap goes on to a new derivation that is valid.

   procedure Derive_Cap
     (Pid     : Process_Id;
      Cap     : Rose.Capabilities.Capability;
      New_Cap : out Rose.Capabilities.Capability;
      Error   : out Rose.Invocation.Invocation_Error);
   --  A copy of Cap, in a new capability of Pid, that is rescinded on its
   --  own.  New_Cap is the null capability, and Error says why, if Cap is
   --  not a capability of Pid or is not valid, if it is eight levels
   --  down already, or if there is no capability or record to make.

   procedure Delete_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability);

   procedure Discard_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability);
   --  Delete Cap, which Pid gives up with the Delete cap. If it is a reply
   --  cap whose caller is waiting for the reply, the call fails with
   --  Closed: a single-use cap is moved, not copied (#864), so no one
   --  else holds it, and the caller would otherwise wait for good (#879).
   --  The caller's buffer is then unmapped from the server that has it.

   procedure Set_Cap
     (Pid    : Process_Id;
      Cap    : Rose.Capabilities.Capability;
      Layout : Rose.Capabilities.Layout.Capability_Layout);

   function Current_State
     (Pid : Process_Id)
      return Process_State;

   function Blocked
     (Pid : Process_Id)
      return Boolean;

   function Is_Valid_Entry
     (Source_Cap          : Rose.Capabilities.Layout.Capability_Layout;
      Destination_Process : Process_Id)
      return Boolean;

   function Is_Valid_Endpoint_Index
     (Pid            : Process_Id;
      Endpoint_Index : Rose.Objects.Endpoint_Index)
      return Boolean;

   function Is_Blocked_On_Endpoint
     (Pid            : Process_Id;
      Endpoint_Index : Rose.Objects.Endpoint_Index)
      return Boolean;

   function Is_Blocked_On_Reply
     (Pid : Process_Id)
      return Boolean;

   procedure Wait_For_Reply (Pid : Process_Id);

   function Create_Endpoint
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Objects.Endpoint_Index;
   --  returns the new endpoint index, or 0 if it can't be created

   function Find_Endpoint
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Objects.Endpoint_Index;

   function Require_Endpoint
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Objects.Endpoint_Index;
   --  returns endpoint index of given endpoint, creates
   --  a new one if necessary

   function Create_Endpoint_Cap
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability;

   function Find_Endpoint_Cap
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability;

   procedure Set_Receive_Cap
     (Pid : Process_Id;
      Endpoint    : Rose.Objects.Endpoint_Index;
      Receive_Cap : Rose.Capabilities.Capability);
   --  Record Receive_Cap as the receive cap of the process's registered
   --  endpoint Endpoint. Nothing is recorded for an index that is not a
   --  registered endpoint.

   function Find_Receive_Cap
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability;

   function Current_Receive_Cap
     (Pid : Process_Id)
     return Rose.Capabilities.Capability;

   procedure Set_Current_State
     (Pid : Process_Id;
      State   : Process_State);

   procedure Send_Cap
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Sender_Cap      : Rose.Capabilities.Capability;
      Receiver_Cap    : Rose.Capabilities.Capability;
      Params          : Rose.Invocation.Invocation_Record);

   procedure Send_To_Endpoint
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Sender_Cap      : Rose.Capabilities.Capability;
      Endpoint        : Rose.Objects.Endpoint_Index;
      Identifier      : Rose.Objects.Capability_Identifier;
      Params          : Rose.Invocation.Invocation_Record);

   procedure Send_Reply
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Params          : Rose.Invocation.Invocation_Record);

   procedure Wait_For_Receiver
     (Waiting_Process_Id   : Process_Id;
      Receiving_Process_Id : Process_Id;
      Endpoint             : Rose.Objects.Endpoint_Index;
      Identifier           : Rose.Objects.Capability_Identifier;
      Params               : Rose.Invocation.Invocation_Record);

   procedure Receive
     (Receiver_Id : Process_Id;
      Params      : Rose.Invocation.Invocation_Record);

   procedure Receive_Any
     (Receiver_Id : Process_Id;
      Params      : Rose.Invocation.Invocation_Record);

   function Has_Queued_Message
     (Process  : Process_Id;
      Endpoint    : Rose.Objects.Endpoint_Index)
      return Boolean;

   procedure Send_Queued_Message
     (Process : Process_Id);

   function Has_Pending_Memory_Request
     (Receiver : Process_Id)
      return Boolean;
   --  True if Receiver is mem, and a page fault, a request for heap
   --  memory or an exit waits for it to receive (#270, #697).

   procedure Send_Pending_Memory_Request;
   --  Give mem the request for heap memory if there is one, or else the
   --  next pending page fault or exit.

   procedure Exit_Process (Pid : Process_Id);
   --  Pid has called the kernel's exit cap. A process that mem knows is
   --  ended by mem, through its Process interface's Exit_Process, as a
   --  program that exits through mem is, so that mem frees its memory
   --  before it kills the process. The kernel ends any other process
   --  itself (#697).

   function Has_Restore_Notice
     (Process  : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean;
   --  True if Process has been restored and resumed, has not yet been
   --  told, and Endpoint is 0 (any) or its Restorable.Restored endpoint.

   procedure Send_Restore_Notice
     (Process : Process_Id);
   --  Send Restorable.Restored to Process.

   --  Signals (ADR 0006, #708). Each process has 32 signal bits. Bits 8
   --  to 31 are its own, and it gives out caps that set them (signal
   --  caps). A bit stays set until the process is told, when it next
   --  receives, on any endpoint or on its Signalled endpoint: one message
   --  from the kernel, Signalled.Signal, carries every bit that is set,
   --  and clears them. Bits 0 to 7 are for named signals, which aren't
   --  implemented yet.

   subtype Own_Signal is Natural range 8 .. 31;

   procedure Raise_Signal
     (Process : Process_Id;
      Bit     : Own_Signal);
   --  Set Bit, and tell Process at once if it is waiting to receive.

   function Has_Pending_Signals
     (Process  : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean;
   --  True if Process has signal bits set, has a Signalled endpoint, and
   --  Endpoint is 0 (any) or that endpoint.

   procedure Send_Signals
     (Process : Process_Id);
   --  Send Signalled.Signal to Process, with its signal bits, and clear
   --  them.

   function Next_Blocked_Sender
     (Receiver_Id : Process_Id;
      Endpoint    : Rose.Objects.Endpoint_Index)
      return Process_Id;
   --  Take the first process waiting to send to Receiver_Id on Endpoint
   --  (any, if 0) off its queue. Its buffer was checked when it called,
   --  but its pages can have changed while it waited: a checkpoint makes
   --  them read-only, and mem can unmap them. So they are checked again
   --  with Check_Buffer_Pages, and a sender that fails is skipped: the
   --  reply cap its call made in Receiver_Id is deleted, and its call is
   --  made again when mem has mapped the page (#768). Null_Process_Id if
   --  no sender is waiting.

   procedure Unblock_And_Send
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Receiver_Params : Rose.Invocation.Invocation_Access);

   procedure Return_Error
     (Params : Rose.Invocation.Invocation_Access;
      Error  : Rose.Invocation.Invocation_Error;
      Data   : Rose.Words.Word := 0);

   function Current_Process_Id return Process_Id;
   function Current_Object_Id return Rose.Objects.Object_Id;

   function Use_Tick return Boolean;
   --  current process uses up a tick.  Return True if it has
   --  no ticks left

   function Is_Valid_Process_Id
     (Id : Process_Id)
      return Boolean;

   function Is_Active_Process_Id
     (Id : Process_Id)
      return Boolean;

   function Is_Available_Process_Id
     (Id : Process_Id)
      return Boolean;

   function Is_Boot_Module
     (Id : Process_Id)
      return Boolean;

   procedure Copy_Cap_Layout
     (From_Process_Id : Process_Id;
      From_Cap        : Rose.Capabilities.Capability;
      To_Process_Id   : Process_Id;
      To_Cap          : Rose.Capabilities.Capability);

   procedure Set_Cap_Id
     (Pid    : Process_Id;
      Cap    : Rose.Capabilities.Capability;
      Cap_Id : Rose.Objects.Capability_Identifier);

   procedure Map_Page
     (Pid           : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      User          : Boolean;
      Managed       : Boolean := False);

   procedure Unmap_Page
     (Pid           : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address);

   procedure Unmap_Invocation_Buffer
     (From_Pid, To_Pid : Process_Id);

   function Is_User_Page
     (Pid          : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Boolean;
   --  True if Virtual_Page is mapped in Pid, and Pid can reach it from
   --  user mode.

   function Is_Writable_User_Page
     (Pid          : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Boolean;
   --  True if Virtual_Page is mapped in Pid, and Pid can write it from
   --  user mode.

   procedure Fault_Call
     (Pid          : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address;
      Is_Mapped    : Boolean;
      Write        : Boolean);
   --  The call that Pid is making needs Virtual_Page of its buffer, which
   --  is not mapped, or is mapped read-only and Write is True (#713).
   --  Give mem the page fault that Pid would have taken if it had used
   --  the page itself, and make the call again when mem resumes Pid. The
   --  call has done nothing yet.

   procedure Check_Reply_Record
     (Pid      : Process_Id;
      Writable : out Boolean);
   --  Pid is about to be given the reply to its call, which is copied
   --  into its invocation record. Writable is True if each page of the
   --  record is a user page that Pid can write. The record was checked
   --  when Pid called, but mem can have unmapped a page since, and a
   --  record in a read-only page passes that check (#715). If not, the
   --  reply is held: Pid is interrupted, mem is given the write fault
   --  that Pid would have taken writing the record itself, and the reply
   --  is copied when mem resumes Pid.
   --
   --  The pages of a writable buffer that Pid lent with the call are
   --  checked in the same way. The server wrote them through its shared
   --  slot, so mem saw no write fault. If a checkpoint has made a page
   --  clean since Pid called, it is read-only again, and the write fault
   --  puts it back on mem's dirty list before Pid can see the data
   --  (#716).

   procedure Check_Buffer_Pages
     (Pid    : Process_Id;
      Params : Rose.Invocation.Invocation_Record;
      Mapped : out Boolean);
   --  Mapped is True if Params sends no buffer, or if each page of its
   --  buffer is a user page of Pid, and writable if the buffer is. If not,
   --  Fault_Call has been called for the first page that is not. The
   --  buffer is in user memory: Rose.Kernel.Invocation checked its range
   --  when Pid called.

   function Has_Checked_Record
     (Pid     : Process_Id;
      Address : Rose.Words.Word_32)
      return Boolean;
   --  True if an invocation record at Address passed the check for Pid,
   --  and no page table has changed since, so that it still would (#717).

   procedure Set_Checked_Record
     (Pid     : Process_Id;
      Address : Rose.Words.Word_32);
   --  Remember that an invocation record at Address has passed the check
   --  for Pid.

   function Mapped_Physical_Page
     (Pid          : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Rose.Addresses.Physical_Page_Address;
   --  The frame that Virtual_Page is mapped to in Pid, or 0 if it is not
   --  present (#768).

   function Directory_Page
     (Pid : Process_Id)
      return Rose.Addresses.Physical_Address;

   procedure Expand_Heap
     (Amount : Rose.Addresses.Physical_Bytes);

   procedure Report_Current_Process;

   procedure Set_Process_Handlers
     (On_Launch     : Rose.Capabilities.Capability;
      On_Page_Fault : Rose.Capabilities.Capability);

   function Have_Process_Handlers return Boolean;

   procedure Handle_Page_Fault
     (Pid               : Process_Id;
      Virtual_Page      : Rose.Addresses.Virtual_Page_Address;
      Is_Mapped         : Boolean;
      Read_Attempt      : Boolean;
      Write_Attempt     : Boolean;
      Execution_Attempt : Boolean);

   procedure Set_Command_Endpoint
     (Pid      : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index);
   --  Note that Endpoint is the command endpoint of Pid, for tracing.

   function Is_Command (Pid : Process_Id) return Boolean;
   --  True if Pid has a command endpoint.

   function Is_Command_Endpoint
     (Pid      : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean;
   --  True if Endpoint is the command endpoint of Pid.

   function Page_Fault_Count
     return Natural;

   --  Replacing a running process (ADR 0032, sections 4 and 6, #862).
   --  Mem holds a process's calls while its new version takes over, and
   --  then swaps the two processes' identities. The state that this
   --  keeps is outside the process table, so a checkpoint never saves
   --  it, and Handover_In_Progress keeps checkpoints out until it is
   --  over.

   procedure Hold_Calls
     (Pid    : Process_Id;
      Except : Rose.Objects.Endpoint_Id);
   --  Calls to Pid wait, as if it were busy, until Release_Calls, except
   --  calls to its endpoint Except, which the new version uses during the
   --  handover. Pid's receives take only calls to Except meanwhile. If
   --  Except is not one of Pid's endpoints, every call waits.

   procedure Release_Calls
     (Pid : Process_Id);
   --  End the hold on Pid's calls, and give Pid the first that waits if
   --  it is already waiting to receive.

   function Is_Held
     (Pid      : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean;
   --  True if a call to Endpoint of Pid has to wait (Hold_Calls).

   function Is_Retired
     (Pid : Process_Id)
      return Boolean;
   --  True if Pid's identity has gone to its new version (Swap_Processes),
   --  so that calls to it fail until it is ended.

   type Activity is (Other, Receiving, Calling);
   --  Receiving: blocked, waiting for a message. Calling: blocked in a
   --  call of its own, waiting for its receiver or for the reply.

   function Current_Activity
     (Pid : Process_Id)
      return Activity;

   procedure Swap_Processes
     (Old_Pid : Process_Id;
      New_Pid : Process_Id;
      Success : out Boolean);
   --  New_Pid takes Old_Pid's object id, and its endpoints at the same
   --  indices, so that every cap that names Old_Pid's objects reaches
   --  New_Pid's objects of the same endpoint ids and identifiers. Old_Pid
   --  is retired, with New_Pid's object id. The calls that wait for
   --  Old_Pid wait for New_Pid, and go on waiting until Release_Calls.
   --  Success is False, and nothing changes, if Old_Pid's calls are not
   --  held, New_Pid lacks one of Old_Pid's endpoints, a message is queued
   --  for Old_Pid, or New_Pid has no room for the endpoints or the caps.

   function Handover_In_Progress return Boolean;
   --  True if any process has its calls held or is retired.

   procedure Enter_Checkpoint;
   procedure Leave_Checkpoint;

   procedure Save_Owner_FPU_State;
   --  Save the registers of the FPU owner into its process table entry,
   --  without changing the owner, so that the entry's state is current
   --  (#429).

   procedure Switch_FPU_State;
   --  Device_Not_Available: save the FPU owner's state, and load the
   --  current process's, or a clean state if it has none (#170).

private

   type Process_Flag is
     (Receive_Any, Receive_Cap, Invoke_Reply, Wait_Reply,
      Interrupt_Resume, Message_Queued, Trace, Boot_Module,
      Persistent, Checkpoint_Page_Fault, Fault_Pending, Restore_Notice,
      Restored_Blocked, Memory_Registered, Exit_Pending,
      Checkpoint_Reply_Held, Call_Restart, Reply_Fault);
   --  Flags are saved in a checkpoint by position, so new ones go last.
   --  Memory_Registered: mem was told about the process when it was
   --  loaded, so mem ends it when it exits. Exit_Pending: it has exited,
   --  and mem is to be told when it next receives (#697). Call_Restart:
   --  the process is interrupted for a page of its call's buffer, and
   --  makes the call again when it resumes, instead of returning from an
   --  interrupt (#713). Reply_Fault: the process is interrupted for a
   --  page of its invocation record, and is given its reply when it
   --  resumes (#715).

   type Process_Flag_Array is array (Process_Flag) of Boolean;

   type Process_Id is new Rose.Words.Word;
   Null_Process_Id : constant Process_Id := 0;
   Next_Pid : Process_Id := 1;
   --  Where New_Process starts looking for a free entry.

   Next_Object_Id : Rose.Objects.Object_Id :=
                      Rose.Objects.First_Process_Object_Id;
   --  The object id of the next process that is not a boot module.  An
   --  object id does not depend on the entry that holds the process, and
   --  is never used again, so a cap naming a process that has gone does
   --  not reach a later one.  A checkpoint saves it, and a restore
   --  carries on from there.

   procedure Assign_Object_Id (Pid : Process_Id);
   --  Give the new process in entry Pid the next object id.

   Max_Registered_Endpoints : constant := 64;
   type Registered_Endpoint_Record is
      record
         Endpoint     : Rose.Objects.Endpoint_Id     := 0;
         Receive_Cap  : Rose.Capabilities.Capability := 0;
         Send_Cap     : Rose.Capabilities.Capability := 0;
      end record;

   subtype Registered_Endpoint_Index is
     Rose.Objects.Endpoint_Index
   range 1 .. Max_Registered_Endpoints;

   type Registered_Endpoint_Array is
     array (Registered_Endpoint_Index) of Registered_Endpoint_Record;

   type Page_Range is
      record
         Base  : Virtual_Page_Address;
         Bound : Virtual_Page_Address;
      end record;

   type Page_Flags is
      record
         Valid         : Boolean;
         Readable      : Boolean;
         Writable      : Boolean;
         Executable    : Boolean;
         Disable_Cache : Boolean;
         Write_Through : Boolean;
         Fixed_Address : Boolean;
      end record;

   Max_Page_Ranges : constant := 8;
   type Page_Range_Index is range 1 .. Max_Page_Ranges;

   Exec_Range_Index       : constant Page_Range_Index := 1;
   Text_Range_Index       : constant Page_Range_Index := 2;
   Data_Range_Index       : constant Page_Range_Index := 3;
   Stack_Range_Index      : constant Page_Range_Index := 4;
   Invocation_Range_Index : constant Page_Range_Index := 5;
   Extra_Range_Index      : constant Page_Range_Index := 6;

   type Page_Range_Array is
     array (Page_Range_Index range 1 .. Max_Page_Ranges) of Page_Range;
   type Page_Flag_Array is
     array (Page_Range_Index range 1 .. Max_Page_Ranges) of Page_Flags;

   type Kernel_Process_Entry;

   type Kernel_Process_Access is access all Kernel_Process_Entry;

   subtype Process_Name is String (1 .. 16);

   type Shared_Page_Record is
      record
         Page : Virtual_Page_Address;
         Pid  : Process_Id;
      end record;

   Max_Shared_Pages : constant := 8;
   type Shared_Page_Count is range 0 .. Max_Shared_Pages;
   subtype Shared_Page_Index is
     Shared_Page_Count range 1 .. Shared_Page_Count'Last;

   type Shared_Page_Array is
     array (Shared_Page_Index) of Shared_Page_Record;

   type Kernel_Process_Entry is
      record
         Stack             : Rose.Kernel.Arch.Stack_Frame;
         Directory_Page    : Rose.Addresses.Physical_Address;
         Pid               : Process_Id;
         Oid               : Rose.Objects.Object_Id;
         Name              : Process_Name;
         State             : Process_State;
         Flags             : Process_Flag_Array := (others => False);
         Default_Cap       : Rose.Capabilities.Capability :=
                               Rose.Capabilities.Null_Capability;
         Receive_Cap       : Rose.Capabilities.Capability :=
                               Rose.Capabilities.Null_Capability;
         Send_Cap          : Rose.Capabilities.Capability :=
                               Rose.Capabilities.Null_Capability;
         Caps              : Rose.Kernel.Cap_Tables.Cap_Table;
         Endpoints         : Registered_Endpoint_Array;
         Page_Ranges       : Page_Range_Array;
         Page_Flags        : Page_Flag_Array;
         Priority          : Process_Priority;
         Quantum_Ticks     : Rose.Words.Word_32 := 10;
         Remaining_Ticks   : Rose.Words.Word_32 := 10;
         Queue_Next        : Kernel_Process_Access;
         Waiting_Next      : Kernel_Process_Access;
         Waiting_First     : Kernel_Process_Access;
         Waiting_Endpoint  : Rose.Objects.Endpoint_Index;
         Waiting_Cap_Id    : Rose.Objects.Capability_Identifier;
         Current_Params    : Rose.Invocation.Invocation_Record;
         Queued_Params     : Rose.Invocation.Invocation_Record;
         Queued_Endpoint   : Rose.Objects.Endpoint_Index;
         Queued_Cap_Id     : Rose.Objects.Capability_Identifier;
         Code_Page         : Rose.Addresses.Physical_Page_Address;
         Data_Page         : Rose.Addresses.Physical_Page_Address;
         Stack_Page        : Rose.Addresses.Physical_Page_Address;
         Invocation_Page   : Rose.Addresses.Physical_Page_Address;
         Checkpoint_Fault  : Rose.Addresses.Virtual_Page_Address;
         Pending_Fault_Page    : Rose.Addresses.Virtual_Page_Address := 0;
         Pending_Fault_Mapped  : Boolean := False;
         Pending_Fault_Write   : Boolean := False;
         Pending_Fault_Execute : Boolean := False;
         --  A page fault that waits for mem to receive (Fault_Pending).
         Lent_Buffer_Page  : Rose.Addresses.Virtual_Page_Address := 0;
         Lent_Buffer_Pages : Natural := 0;
         --  The writable buffer of the call that the process waits on. The
         --  server writes it through its shared slot, which mem doesn't
         --  see, so Check_Reply_Record checks these pages too (#716).
         Shared_Pages      : Shared_Page_Array;
         Saved_Stack_Value : Rose.Words.Word_32;
         Command_Endpoint  : Rose.Objects.Endpoint_Index := 0;
         --  The command endpoint of a command (ADR 0002), or zero.
         Faults_At_Create  : Natural := 0;
         --  The page fault count when the process was created.
         FPU_State         : Rose.Kernel.Arch.FPU_State;
         FPU_Valid         : Boolean := False;
         --  The process's FPU and SSE registers, valid once it has used
         --  them and another process has used them since (#170).
         Signals           : Rose.Words.Word_32 := 0;
         --  The signal bits that are set and not yet delivered (#708).
      end record;

   type Kernel_Process_Table is
     array (Process_Id
            range 1 .. Rose.Kernel.Limits.Max_Processes)
     of aliased Kernel_Process_Entry;

   type Process_Table_Access is
     access all Kernel_Process_Table;

   Process_Table : Process_Table_Access;

   Derivation_Records : Rose.Kernel.Derivations.Table;
   --  What the capabilities that were derived name (ADR 0001).  A slot
   --  that holds a layout with a derivation counts as a reference to its
   --  record, which Set_Cap, Delete_Cap and Release_Entry keep.

   procedure Create_Process_Table_Entry
     (Pid : Rose.Kernel.Processes.Process_Id;
      Name : String);
   --  Set up the entry for a new process Pid, with its page directory,
   --  invocation buffer page and standard page ranges, ready to start.

   Process_Table_Heap_Size : constant Rose.Addresses.Physical_Bytes :=
                               Kernel_Process_Table'Size / 8
                                 + Rose.Addresses.Physical_Bytes
                                     (Rose.Kernel.Limits.Max_Processes)
                                 * Rose.Addresses.Physical_Page_Bytes;
   Current_Process : Kernel_Process_Access;
   pragma Export (C, Current_Process, "current_process_ptr");

   Next_Process : Kernel_Process_Access;
   pragma Export (C, Next_Process, "next_process_ptr");

   Invocation_Reply : Words.Word_32;
   pragma Export (C, Invocation_Reply, "invoke_reply");

   Interrupted_Resume : Words.Word_32 := 0;
   pragma Export (C, Interrupted_Resume, "interrupt_resume");

   FPU_Owner : Kernel_Process_Access := null;
   pragma Export (C, FPU_Owner, "fpu_owner_ptr");
   --  The process whose FPU and SSE state is in the registers. Returning
   --  to any other process sets CR0.TS (mp.s), so that its first FPU or
   --  SSE instruction calls Switch_FPU_State.

   Process_Invocation_Record : aliased Rose.Invocation.Invocation_Record;
   pragma Export (C, Process_Invocation_Record, "invocation_record");

   Saved_Process_Address : Rose.Words.Word_32;
   pragma Export (C, Saved_Process_Address, "saved_process_address");

   Idle_State : Rose.Words.Word_32;
   pragma Export (C, Idle_State, "idle_state");

   function Current_Process_Id return Rose.Kernel.Processes.Process_Id
   is (if Current_Process = null then 0 else Current_Process.Pid);

   function Is_Valid_Process_Id
     (Id : Rose.Kernel.Processes.Process_Id)
      return Boolean
   is (Id in Process_Table'Range);

   function Is_Active_Process_Id
     (Id : Rose.Kernel.Processes.Process_Id)
      return Boolean
   is (Id in Process_Table'Range
       and then Process_Table (Id).State /= Available);

   function Is_Available_Process_Id
     (Id : Rose.Kernel.Processes.Process_Id)
      return Boolean
   is (Id in Process_Table'Range
       and then Process_Table (Id).State = Available);

   function Current_State
     (Pid : Process_Id)
      return Process_State
   is (Process_Table (Pid).State);

   function Blocked
     (Pid : Process_Id)
      return Boolean
   is (Current_State (Pid) = Blocked);

   function Current_Receive_Cap
     (Pid : Process_Id)
      return Rose.Capabilities.Capability
   is (Process_Table (Pid).Receive_Cap);

   function Create_Cap return Rose.Capabilities.Capability
   is (Create_Cap (Current_Process_Id));

   procedure Load_Cap_Page
     (Pid  : Process_Id;
      Page : Rose.Kernel.Cap_Tables.Capability_Page_Index);
   --  Allocate the page of Pid's capabilities, if it has not got it, for
   --  Set_Cap, Rescind_Cap and Set_Cap_Id, which need one.  Reading a
   --  capability, and deleting one, do not.

   function Cap_Layout
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Layout;

   function To_Object_Id
     (Pid : Process_Id)
      return Rose.Objects.Object_Id
   is (Process_Table (Pid).Oid);

   function Current_Object_Id return Rose.Objects.Object_Id
   is (Current_Process.Oid);

   function Trace (Process : Process_Id) return Boolean
   is (Is_Valid_Process_Id (Process)
       and then Process_Table (Process).Flags (Trace));

   function Directory_Page
     (Pid : Process_Id)
      return Rose.Addresses.Physical_Address
   is (Process_Table (Pid).Directory_Page);

   function Is_Boot_Module
     (Id : Process_Id)
      return Boolean
   is (Process_Table (Id).Flags (Boot_Module));

   function Is_Blocked_On_Reply
     (Pid : Process_Id)
      return Boolean
   is (Process_Table (Pid).Flags (Wait_Reply));

--     function Has_Queued_Message
--       (Process : Process_Id)
--        return Boolean
--     is (Process_Table (Process).Flags (Message_Queued));

   function Handle_General_Protection_Fault
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status;

   procedure Restart_Call (P : in out Kernel_Process_Entry);
   --  Set P to make the system call it is in again when it next returns
   --  to user mode, with no reply.

   Mem_Process       : Rose.Kernel.Processes.Process_Id := 0;
   Mem_Launch_Cap     : Rose.Capabilities.Capability := 0;
   Mem_Page_Fault_Cap : Rose.Capabilities.Capability := 0;

end Rose.Kernel.Processes;
