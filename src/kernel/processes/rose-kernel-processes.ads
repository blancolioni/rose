with Rose.Addresses;
with Rose.Capabilities.Layout;
with Rose.Invocation;
with Rose.Words;

private with Rose.Kernel.Arch;
private with Rose.Kernel.Interrupts;
private with Rose.Kernel.Limits;

package Rose.Kernel.Processes is

   Process_Table_Heap_Size : constant Rose.Addresses.Physical_Bytes;
   Process_Priority_Count  : constant := 16;

   type Process_State is
     (Available, Ready, Running, Blocked, Interrupted, Killed);

   type Process_Priority is range 1 .. Process_Priority_Count;

   function Current_Process_Cap
     (Cap_Index : Rose.Capabilities.Capability;
      Cap       : out Rose.Capabilities.Layout.Capability_Layout)
      return Boolean;

   procedure Set_Current_Invocation
     (Invocation : Rose.Invocation.Invocation_Record);

   function Create_Cap
     (Process_Id : Rose.Objects.Process_Id)
      return Rose.Capabilities.Capability;

   function Create_Caps
     (Process_Id : Rose.Objects.Process_Id;
      Count      : Natural)
      return Rose.Capabilities.Capability;
   --  Create Count caps and return the first.  They
   --  are guaranteed to be sequential, or the return
   --  value is Null_Capability

   function Has_Cap
     (Process_Id : Rose.Objects.Process_Id;
      Cap        : Rose.Capabilities.Capability)
      return Boolean;

   function Cap_Type
     (Process_Id : Rose.Objects.Process_Id;
      Cap        : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Type;

   procedure Delete_Cap
     (Process_Id : Rose.Objects.Process_Id;
      Cap        : Rose.Capabilities.Capability);

   procedure Set_Cap
     (Process_Id : Rose.Objects.Process_Id;
      Cap        : Rose.Capabilities.Capability;
      Layout     : Rose.Capabilities.Layout.Capability_Layout);

   function Current_State
     (Process : Rose.Objects.Process_Id)
      return Process_State;

   function Blocked
     (Process : Rose.Objects.Process_Id)
      return Boolean;

   function Is_Valid_Endpoint_Index
     (Process        : Rose.Objects.Process_Id;
      Endpoint_Index : Rose.Objects.Endpoint_Index)
      return Boolean;

   function Is_Blocked_On_Endpoint
     (Process        : Rose.Objects.Process_Id;
      Endpoint_Index : Rose.Objects.Endpoint_Index)
      return Boolean;

   function Create_Endpoint
     (Process_Id : Rose.Objects.Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Objects.Endpoint_Index;
   --  returns the new endpoint index, or 0 if it can't be created

   procedure Set_Endpoint_Caps
     (Process_Id  : Rose.Objects.Process_Id;
      Endpoint    : Rose.Objects.Endpoint_Index;
      Receive_Cap : Rose.Capabilities.Capability;
      Send_Cap    : Rose.Capabilities.Capability);

   function Find_Endpoint_Cap
     (Process_Id : Rose.Objects.Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability;

   function Current_Receive_Cap
     (Process : Rose.Objects.Process_Id)
     return Rose.Capabilities.Capability;

   procedure Set_Current_State
     (Process : Rose.Objects.Process_Id;
      State   : Process_State);

   procedure Send_Cap
     (From_Process    : Rose.Objects.Process_Id;
      To_Process      : Rose.Objects.Process_Id;
      Sender_Cap      : Rose.Capabilities.Capability;
      Receiver_Cap    : Rose.Capabilities.Capability;
      Params          : Rose.Invocation.Invocation_Record);

   procedure Send_To_Endpoint
     (From_Process    : Rose.Objects.Process_Id;
      To_Process      : Rose.Objects.Process_Id;
      Sender_Cap      : Rose.Capabilities.Capability;
      Endpoint        : Rose.Objects.Endpoint_Index;
      Identifier      : Rose.Objects.Capability_Identifier;
      Params          : Rose.Invocation.Invocation_Record);

   procedure Send_Reply
     (From_Process : Rose.Objects.Process_Id;
      To_Process   : Rose.Objects.Process_Id;
      Params       : Rose.Invocation.Invocation_Record);

   procedure Wait_For_Receiver
     (Waiting_Process   : Rose.Objects.Process_Id;
      Receiving_Process : Rose.Objects.Process_Id;
      Endpoint          : Rose.Objects.Endpoint_Index;
      Identifier        : Rose.Objects.Capability_Identifier;
      Params            : Rose.Invocation.Invocation_Record);

   procedure Receive
     (Receiver : Rose.Objects.Process_Id;
      Params   : Rose.Invocation.Invocation_Record);

   procedure Receive_Any
     (Receiver : Rose.Objects.Process_Id;
      Params   : Rose.Invocation.Invocation_Record);

   function Next_Blocked_Sender
     (Receiver : Rose.Objects.Process_Id)
      return Rose.Objects.Process_Id;

   procedure Unblock_And_Send
     (From_Process    : Rose.Objects.Process_Id;
      To_Process      : Rose.Objects.Process_Id;
      Receiver_Params : Rose.Invocation.Invocation_Access);

   procedure Return_Error
     (Params : Rose.Invocation.Invocation_Access;
      Error  : Rose.Invocation.Invocation_Error);

   function Current_Process_Id return Rose.Objects.Process_Id;

   function Use_Tick return Boolean;
   --  current process uses up a tick.  Return True if it has
   --  no ticks left

   function Get_Page_Object_Id
     (Process : Rose.Objects.Process_Id;
      Address : Rose.Words.Word)
      return Rose.Objects.Page_Object_Id;
   --  convert to (Process, Address) pair to a page object id

   function Get_Page_Object_Process
     (Page_Object : Rose.Objects.Page_Object_Id)
      return Rose.Objects.Process_Id;

   function Get_Page_Object_Page
     (Page_Object : Rose.Objects.Page_Object_Id)
      return Rose.Addresses.Virtual_Page_Address;

   function Is_Valid_Process_Id
     (Id : Rose.Objects.Process_Id)
      return Boolean;

   function Is_Active_Process_Id
     (Id : Rose.Objects.Process_Id)
      return Boolean;

   function Is_Available_Process_Id
     (Id : Rose.Objects.Process_Id)
      return Boolean;

   procedure Copy_Cap_Layout
     (From_Process : Rose.Objects.Process_Id;
      From_Cap     : Rose.Capabilities.Capability;
      To_Process   : Rose.Objects.Process_Id;
      To_Cap       : Rose.Capabilities.Capability);

   procedure Set_Cap_Id
     (Process      : Rose.Objects.Process_Id;
      Cap          : Rose.Capabilities.Capability;
      Id           : Rose.Objects.Capability_Identifier);

   procedure Map_Page
     (Process       : Rose.Objects.Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      User          : Boolean);

   procedure Unmap_Page
     (Process       : Rose.Objects.Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address);

   procedure Unmap_Invocation_Buffer
     (Process       : Rose.Objects.Process_Id);

   function Mapped_Physical_Page
     (Process       : Rose.Objects.Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address)
      return Rose.Addresses.Physical_Page_Address;

   procedure Report_Current_Process;

   procedure Set_Process_Handlers
     (On_Launch     : Rose.Capabilities.Capability;
      On_Kill       : Rose.Capabilities.Capability;
      On_Page_Fault : Rose.Capabilities.Capability);

   function Have_Process_Handlers return Boolean;

   procedure Handle_Page_Fault
     (Virtual_Page      : Rose.Addresses.Virtual_Page_Address;
      Is_Mapped         : Boolean;
      Read_Attempt      : Boolean;
      Write_Attempt     : Boolean;
      Execution_Attempt : Boolean);

   function Page_Fault_Count
     return Natural;

   function Current_ESP
     (Process : Rose.Objects.Process_Id)
      return Rose.Words.Word_32;

   function Current_EIP
     (Process : Rose.Objects.Process_Id)
      return Rose.Words.Word_32;

private

   type Process_Flag is
     (Receive_Any, Receive_Caps, Receive_Cap, Invoke_Reply, Interrupt_Resume);

   type Process_Flag_Array is array (Process_Flag) of Boolean;

   Cached_Capability_Count : constant := 64;

   type Capability_Cache_Array is
     array (Rose.Capabilities.Capability range 1 .. Cached_Capability_Count)
     of Rose.Capabilities.Layout.Capability_Layout;

   Capabilities_Per_Page : constant :=
                   Rose.Addresses.Physical_Page_Bytes
                 / Rose.Capabilities.Layout.Capability_Layout_Bytes;

   type Capability_Page is
     array (Rose.Capabilities.Capability range 1 .. Capabilities_Per_Page)
     of Rose.Capabilities.Layout.Capability_Layout
       with Size => Rose.Addresses.Physical_Page_Bits;

   Max_Capability_Pages : constant := 16;
   Max_Capability_Index    : constant :=
                               Cached_Capability_Count
                                 + Capabilities_Per_Page
                               * Max_Capability_Pages;

   type Capability_Page_Array is
     array (1 .. Max_Capability_Pages) of Rose.Addresses.Physical_Page_Address;

   Max_Registered_Endpoints : constant := 32;
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

   type Kernel_Process_Entry is
      record
         Stack             : Rose.Kernel.Arch.Stack_Frame;
         Directory_Page    : Rose.Addresses.Physical_Address;
         Pid               : Rose.Objects.Process_Id;
         Name              : Process_Name;
         State             : Process_State;
         Flags             : Process_Flag_Array := (others => False);
         Default_Cap       : Rose.Capabilities.Capability :=
                               Rose.Capabilities.Null_Capability;
         Receive_Cap       : Rose.Capabilities.Capability :=
                               Rose.Capabilities.Null_Capability;
         Send_Cap          : Rose.Capabilities.Capability :=
                               Rose.Capabilities.Null_Capability;
         Cap_Cache         : Capability_Cache_Array;
         Cap_Pages         : Capability_Page_Array;
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
         Code_Page         : Rose.Addresses.Physical_Page_Address;
         Data_Page         : Rose.Addresses.Physical_Page_Address;
         Stack_Page        : Rose.Addresses.Physical_Page_Address;
         Env_Page          : Rose.Addresses.Physical_Page_Address;
         Invocation_Buffer : Rose.Addresses.Virtual_Page_Address;
         Saved_Stack_Value : Rose.Words.Word_32;
      end record;

   type Kernel_Process_Table is
     array (Rose.Objects.Process_Id
            range 1 .. Rose.Kernel.Limits.Max_Processes)
     of aliased Kernel_Process_Entry;

   type Process_Table_Access is
     access all Kernel_Process_Table;

   Process_Table : Process_Table_Access;

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

   Process_Invocation_Record : aliased Rose.Invocation.Invocation_Record;
   pragma Export (C, Process_Invocation_Record, "invocation_record");

   Saved_Process_Address : Rose.Words.Word_32;
   pragma Export (C, Saved_Process_Address, "saved_process_address");

   Idle_State : Rose.Words.Word_32;
   pragma Export (C, Idle_State, "idle_state");

   function Current_Process_Id return Rose.Objects.Process_Id
   is (if Current_Process = null then 0 else Current_Process.Pid);

   function Is_Valid_Process_Id
     (Id : Rose.Objects.Process_Id)
      return Boolean
   is (Id in Process_Table'Range);

   function Is_Active_Process_Id
     (Id : Rose.Objects.Process_Id)
      return Boolean
   is (Id in Process_Table'Range
       and then Process_Table (Id).State /= Available);

   function Is_Available_Process_Id
     (Id : Rose.Objects.Process_Id)
      return Boolean
   is (Id in Process_Table'Range
       and then Process_Table (Id).State = Available);

   function Current_State
     (Process : Rose.Objects.Process_Id)
      return Process_State
   is (Process_Table (Process).State);

   function Blocked
     (Process : Rose.Objects.Process_Id)
      return Boolean
   is (Current_State (Process) = Blocked);

   function Current_Receive_Cap
     (Process : Rose.Objects.Process_Id)
      return Rose.Capabilities.Capability
   is (Process_Table (Process).Receive_Cap);

   function Create_Cap return Rose.Capabilities.Capability
   is (Create_Cap (Current_Process_Id));

   function Handle_General_Protection_Fault
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status;

   Mem_Process        : Rose.Objects.Process_Id := 0;
   Mem_Launch_Cap     : Rose.Capabilities.Capability := 0;
   Mem_Kill_Cap       : Rose.Capabilities.Capability := 0;
   Mem_Page_Fault_Cap : Rose.Capabilities.Capability := 0;

end Rose.Kernel.Processes;
