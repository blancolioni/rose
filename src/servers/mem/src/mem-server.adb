
with Rose.Addresses;
with Rose.Capabilities;
with Rose.Invocation;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;
with Rose.Trace;

with Rose.Interfaces.Processes.Heap.Server;
with Rose.Interfaces.Processes.Memory.Server;
with Rose.Interfaces.Checkpoints.Memory_Restore.Server;

with Rose.Interfaces.Processes.Process.Server;
with Rose.Interfaces.Processes.Process_Control.Server;
with Rose.Interfaces.Caps.Receiver.Server;
with Rose.Interfaces.Processes.Replacement.Server;
with Rose.Interfaces.Processes.Replacer.Server;
with Rose.Interfaces.Processes.Segment;


with Rose.Server;
with Rose.System_Calls;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Mem.Calls;
with Mem.Change_Table;
with Mem.Checkpoints;
with Mem.Cleaning;
with Mem.Page_Cache;
with Mem.Page_Log;
with Mem.Page_Table;
with Mem.Physical_Map;
with Mem.Processes;
with Mem.Shared_Pages;
with Mem.Virtual_Map;

package body Mem.Server is

   Server : Rose.Server.Server_Context;

   --  Replacing a running process (ADR 0032, #866). The caller drives
   --  the replacement, since mem can't wait for the new version while it
   --  has page faults to serve: mem holds the old version's calls, lets
   --  the new version's through, and makes the switch.

   Hold_Calls_Cap     : Rose.Capabilities.Capability := 0;
   Release_Calls_Cap  : Rose.Capabilities.Capability := 0;
   Swap_Processes_Cap : Rose.Capabilities.Capability := 0;
   Process_State_Cap  : Rose.Capabilities.Capability := 0;
   --  The kernel's handover caps (#862), which init gives mem.


   Request_Checkpoint_Cap : Rose.Capabilities.Capability := 0;
   --  The checkpoint server's Checkpointer.Request_Checkpoint, from the
   --  last checkpoint, for asking for a checkpoint sooner than the timer
   --  would (ADR 0028, section 6).

   Checkpoint_Requested : Boolean := False;
   --  mem has asked for a checkpoint, and it hasn't happened yet.

   type Held_Fault is
      record
         Object  : Rose.Objects.Object_Id;
         Virtual : Rose.Words.Word;
         Action  : Rose.Interfaces.Processes.Memory.Page_Access_Type;
      end record;

   Held_Faults : array (1 .. Rose.Limits.Max_Processes) of Held_Fault;
   Held_Count  : Natural := 0;
   --  Faults that found no page, with a checkpoint on the way that will
   --  let mem free pages again. A process has one fault at a time. They
   --  are handled again when mem's part of the checkpoint is done (ADR
   --  0028, section 6; #974).

   procedure Ask_For_Checkpoint;
   --  If mem needs a checkpoint, to empty its table of changes or to
   --  page out again, or for its held faults, and hasn't asked for one,
   --  ask now, without waiting for a reply. The checkpoint can start at
   --  once, so this is the last thing mem does before it waits for its
   --  next message (#950).

   procedure Handle_Fault
     (Object   : Rose.Objects.Object_Id;
      Virtual  : Rose.Words.Word;
      Physical : Rose.Words.Word;
      Action   : Rose.Interfaces.Processes.Memory.Page_Access_Type;
      May_Wait : Boolean);
   --  Page_Fault, or a held fault handled again. A fault that finds no
   --  page is held if May_Wait and a checkpoint will free pages, and
   --  otherwise fails its process.

   procedure Replay_Held_Faults;
   --  Handle the held faults again, after mem's part of a checkpoint.

   type Replacement_Record is
      record
         Active          : Boolean := False;
         Committed       : Boolean := False;
         Identifier      : Rose.Objects.Capability_Identifier := 0;
         Replacement_Cap : Rose.Capabilities.Capability := 0;
         Old_Cap         : Rose.Capabilities.Capability := 0;
         Old_Pid         : Process_Id := 0;
         New_Cap         : Rose.Capabilities.Capability := 0;
         New_Oid         : Rose.Objects.Object_Id := 0;
      end record;
   --  The one replacement in progress. Old_Cap and New_Cap name the two
   --  processes to the kernel: caps to their objects, which the caller
   --  gave. Mem's slot of the new version is found by New_Oid, since
   --  the process can end before the replacement does.

   Current_Replacement  : Replacement_Record;
   Last_Replacement_Id  : Rose.Objects.Capability_Identifier := 0;

   procedure Kernel_Activity
     (Cap      : Rose.Capabilities.Capability;
      Activity : out Rose.Words.Word;
      Oid      : out Rose.Objects.Object_Id);
   --  What the process that Cap names is doing (the kernel's
   --  Process_State: 1 receiving, 2 in a call, 0 otherwise), and its
   --  object id, or 0 for both if Cap names no process.

   function Kernel_Handover
     (Op     : Rose.Capabilities.Capability;
      First  : Rose.Capabilities.Capability;
      Second : Rose.Capabilities.Capability := 0)
      return Boolean;
   --  Invoke the kernel's handover cap Op on the processes that First
   --  and, if it isn't null, Second name. False if the kernel refused.

   function Is_Current
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean
   is (Current_Replacement.Active
       and then Rose.Objects."=" (Id, Current_Replacement.Identifier));

   procedure End_Replacement;
   --  Forget the current replacement, and delete its caps.

   function Replace_Process
     (Id  : Rose.Objects.Capability_Identifier;
      Old : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   function Replacement_Ready
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean;

   function Replacement_Bank
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure Allow_New_Version
     (Id          : Rose.Objects.Capability_Identifier;
      New_Version : Rose.Capabilities.Capability);

   function Commit_Replacement
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean;

   procedure Release_Replacement
     (Id : Rose.Objects.Capability_Identifier);

   function New_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Process     : Rose.Capabilities.Capability;
      Bank        : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   procedure Set_Process_Banks
     (Id    : Rose.Objects.Capability_Identifier;
      Banks : Rose.Capabilities.Capability);

   procedure Set_Page_Log
     (Id  : Rose.Objects.Capability_Identifier;
      Log : Rose.Capabilities.Capability);

   procedure Processes_Ended (Id : Rose.Objects.Capability_Identifier);

   procedure Log_Committed (Id : Rose.Objects.Capability_Identifier);

   function Process_Bank
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure End_Process
     (Pid       : Process_Id;
      Bank_Gone : Boolean);

   procedure Drain_Ended;
   --  End every process that the store has queued, because their banks
   --  were destroyed.
   --  End process Pid, and unless its bank is gone already, tell the store
   --  to destroy it (ADR 0015, #750).

   procedure Exit_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Exit_Status : Natural);

   procedure Register_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Process     : Rose.Capabilities.Capability;
      Exec_Base   : Rose.Words.Word;
      Exec_Bound  : Rose.Words.Word;
      Text_Base   : Rose.Words.Word;
      Text_Bound  : Rose.Words.Word;
      Data_Base   : Rose.Words.Word;
      Data_Bound  : Rose.Words.Word;
      Stack_Base  : Rose.Words.Word;
      Stack_Bound : Rose.Words.Word);

   procedure Take_Physical_Memory
     (Id     : in     Rose.Objects.Capability_Identifier;
      Size   : in     Rose.Words.Word;
      Start  :    out Rose.Words.Word;
      Amount :    out Rose.Words.Word);

   procedure Add_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Capabilities.Capability;
      Offset        : Rose.Words.Word;
      Flags         : Rose.Words.Word);

   procedure Add_Private_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Capabilities.Capability;
      Flags         : Rose.Words.Word);
   --  The process's own region, which it is handed: mem releases it when
   --  the segment goes, or the process is gone (#403).

   procedure Add_Nonpersistent_Segment
     (Id            : in     Rose.Objects.Capability_Identifier;
      Virtual_Base  : in     Rose.Words.Word;
      Virtual_Bound : in     Rose.Words.Word;
      Flags         : in     Rose.Words.Word);

   procedure Checkpoint
     (Id                 : Rose.Objects.Capability_Identifier;
      Request_Checkpoint : Rose.Capabilities.Capability);

   function Published_Interface
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Control_Cap
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;
   --  The cap that the process's launcher holds (#99).

   function Heap_Interface
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Current_Bound
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word;

   procedure Request_New_Bound
     (Id        : Rose.Objects.Capability_Identifier;
      New_Bound : Rose.Words.Word);

   procedure Publish_Interface
     (Id            : Rose.Objects.Capability_Identifier;
      Interface_Cap : Rose.Capabilities.Capability);

   procedure Send_Cap
     (Id  : Rose.Objects.Capability_Identifier;
      Cap : Rose.Capabilities.Capability)
      renames Publish_Interface;

   procedure Page_Fault
     (Id       : Rose.Objects.Capability_Identifier;
      Object   : Rose.Objects.Object_Id;
      Virtual  : Rose.Words.Word;
      Physical : Rose.Words.Word;
      Action   : Rose.Interfaces.Processes.Memory.Page_Access_Type);

   procedure Destroy (Id : Rose.Objects.Capability_Identifier);

   procedure Remove_Segment
     (Id           : Rose.Objects.Capability_Identifier;
      Virtual_Base : Rose.Words.Word);

   procedure Shared_Page_Fault
     (Object        : Rose.Objects.Object_Id;
      Pid           : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      Store_Page    : Rose.Objects.Object_Id;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Start         : Natural);
   --  A fault on a page of a shared segment that isn't mapped: map the
   --  physical page that holds Store_Page, reading it from the region if
   --  no process has it (ADR 0025).

   procedure Remove_Instances (Id : Rose.Objects.Capability_Identifier);
   --  Delete the caps that the servers made for process Id, which is
   --  gone, so that its entries in their instance tables are free.

   procedure Protection_Fault
     (Process : Rose.Objects.Object_Id;
      Address : Rose.Addresses.Virtual_Page_Address;
      Message : String);

   -------------------------------
   -- Add_Nonpersistent_Segment --
   -------------------------------

   procedure Add_Nonpersistent_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Flags         : Rose.Words.Word)
   is
      use Rose.Interfaces.Processes.Segment;
      use type Rose.Words.Word;

      Pid   : constant Process_Id := Mem.Processes.Find_Process (Id);
      Added : Boolean;
   begin
      if Pid = 0 then
         Rose.Server.Fail;
         return;
      end if;

      Mem.Processes.Add_Nonpersistent_Segment
        (Process      => Pid,
         Virtual_Base => Rose.Addresses.Virtual_Page_Address (Virtual_Base),
         Virtual_Bound => Rose.Addresses.Virtual_Page_Address (Virtual_Bound),
         Readable     => (Flags and Segment_Readable) /= 0,
         Writable     => (Flags and Segment_Writable) /= 0,
         Executable   => (Flags and Segment_Executable) /= 0,
         Resizable    => (Flags and Segment_Resizable) /= 0,
         Success      => Added);

      if not Added then
         Rose.Server.Fail;
      end if;
   end Add_Nonpersistent_Segment;

   -------------------------
   -- Add_Private_Segment --
   -------------------------

   procedure Add_Private_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Capabilities.Capability;
      Flags         : Rose.Words.Word)
   is
      use Rose.Interfaces.Processes.Segment;
      use type Rose.Words.Word;

      Pid    : constant Process_Id := Mem.Processes.Find_Process (Id);
      Added  : Boolean;
   begin
      if Pid = 0 then
         Rose.System_Calls.Delete_Cap (Region);
         Rose.Server.Fail;
         return;
      end if;

      Mem.Processes.Add_Segment
        (Process       => Pid,
         Virtual_Base  => Rose.Addresses.Virtual_Page_Address (Virtual_Base),
         Virtual_Bound => Rose.Addresses.Virtual_Page_Address (Virtual_Bound),
         Region        => Region,
         Region_Offset => 0,
         Readable      => (Flags and Segment_Readable) /= 0,
         Writable      => (Flags and Segment_Writable) /= 0,
         Executable    => (Flags and Segment_Executable) /= 0,
         Resizable    => (Flags and Segment_Resizable) /= 0,
         Success       => Added,
         Private_Region => True,
         Shared        => (Flags and Segment_Shared) /= 0);

      if not Added then
         Rose.Server.Fail;
      end if;
   end Add_Private_Segment;

   -----------------
   -- Add_Segment --
   -----------------

   procedure Add_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Capabilities.Capability;
      Offset        : Rose.Words.Word;
      Flags         : Rose.Words.Word)
   is
      use Rose.Interfaces.Processes.Segment;
      use type Rose.Words.Word;

      Pid    : constant Process_Id := Mem.Processes.Find_Process (Id);
      Added  : Boolean;
   begin
      if Pid = 0 then
         Rose.System_Calls.Delete_Cap (Region);
         Rose.Server.Fail;
         return;
      end if;

      Mem.Processes.Add_Segment
        (Process       => Pid,
         Virtual_Base  => Rose.Addresses.Virtual_Page_Address (Virtual_Base),
         Virtual_Bound => Rose.Addresses.Virtual_Page_Address (Virtual_Bound),
         Region        => Region,
         Region_Offset => Offset,
         Readable      => (Flags and Segment_Readable) /= 0,
         Writable      => (Flags and Segment_Writable) /= 0,
         Executable    => (Flags and Segment_Executable) /= 0,
         Resizable    => (Flags and Segment_Resizable) /= 0,
         Success       => Added,
         Private_Region => False,
         Shared        => (Flags and Segment_Shared) /= 0);

      if not Added then
         Rose.Server.Fail;
      end if;
   end Add_Segment;

   -----------------------
   -- Allow_New_Version --
   -----------------------

   procedure Allow_New_Version
     (Id          : Rose.Objects.Capability_Identifier;
      New_Version : Rose.Capabilities.Capability)
   is
      use type Rose.Objects.Object_Id;
      R        : Replacement_Record renames Current_Replacement;
      Activity : Rose.Words.Word;
      Oid      : Rose.Objects.Object_Id;
   begin
      if not Is_Current (Id) or else R.Committed then
         Rose.System_Calls.Delete_Cap (New_Version);
         return;
      end if;

      Kernel_Activity (New_Version, Activity, Oid);
      if Oid = 0
        or else not Mem.Processes.Is_Launched
          (Mem.Processes.Get_Process_Id (Oid))
        or else Oid = Mem.Processes.Process_Object_Id (R.Old_Pid)
        or else not Kernel_Handover (Hold_Calls_Cap, R.Old_Cap, New_Version)
      then
         Rose.System_Calls.Delete_Cap (New_Version);
         return;
      end if;

      Rose.System_Calls.Delete_Cap (R.New_Cap);
      R.New_Cap := New_Version;
      R.New_Oid := Oid;
   end Allow_New_Version;

   ------------------------
   -- Ask_For_Checkpoint --
   ------------------------

   procedure Ask_For_Checkpoint is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Checkpoint_Requested
        or else Request_Checkpoint_Cap = 0
        or else not (Held_Count > 0
                     or else Mem.Change_Table.Wants_Checkpoint
                     or else Mem.Virtual_Map.Changes_Stopped)
      then
         return;
      end if;

      Params.Control.Flags := (Rose.Invocation.Send => True,
                               others              => False);
      Params.Cap := Request_Checkpoint_Cap;
      Rose.System_Calls.Invoke_Capability (Params);
      Checkpoint_Requested := True;
   end Ask_For_Checkpoint;

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint
     (Id                 : Rose.Objects.Capability_Identifier;
      Request_Checkpoint : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
      use type Rose.Capabilities.Capability;
   begin
      if Request_Checkpoint /= Request_Checkpoint_Cap then
         if Request_Checkpoint_Cap /= 0 then
            Rose.System_Calls.Delete_Cap (Request_Checkpoint_Cap);
         end if;
         Request_Checkpoint_Cap := Request_Checkpoint;
      end if;

      Mem.Cleaning.Prepare_Checkpoint;
      Mem.Checkpoints.Checkpoint;

      --  Whether it asked for this one or not, mem can page out again,
      --  and the held faults can find pages: the changed pages are clean,
      --  and the table of changes is empty, unless the checkpoint failed.

      Checkpoint_Requested := False;
      Mem.Virtual_Map.End_Checkpoint;
      Replay_Held_Faults;
   end Checkpoint;

   ------------------------
   -- Commit_Replacement --
   ------------------------

   function Commit_Replacement
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean
   is
      use type Rose.Objects.Object_Id;
      use type Rose.Words.Word;
      R        : Replacement_Record renames Current_Replacement;
      New_Pid  : Process_Id;
      Old_Oid  : Rose.Objects.Object_Id;
      Activity : Rose.Words.Word;
      Unused   : Rose.Objects.Object_Id;
   begin
      if not Is_Current (Id) or else R.Committed or else R.New_Oid = 0 then
         return False;
      end if;

      New_Pid := Mem.Processes.Get_Process_Id (R.New_Oid);
      if New_Pid = 0 then
         return False;
      end if;

      --  The old version must be between calls (ADR 0032, section 4).
      --  The kernel checks the rest, and changes nothing if it refuses.

      Kernel_Activity (R.Old_Cap, Activity, Unused);
      if Activity /= 1 then
         return False;
      end if;

      Old_Oid := Mem.Processes.Process_Object_Id (R.Old_Pid);

      if not Kernel_Handover (Swap_Processes_Cap, R.Old_Cap, R.New_Cap) then
         return False;
      end if;

      --  Committed: nothing from here on can be refused (section 7).
      --  The new version answers the old one's object id now, and the old
      --  version has the new one's launch-time id, so mem's records and
      --  the store's bank owners follow them. Old_Cap names the old
      --  object id, which is the new version's now, so releasing its
      --  calls releases the new version's.

      Mem.Processes.Swap_Records (R.Old_Pid, New_Pid);
      Mem.Processes.Hand_Over_Banks (Old_Oid, R.New_Oid);

      if not Kernel_Handover (Release_Calls_Cap, R.Old_Cap) then
         Rose.Console_IO.Put_Line ("mem: replacement: cannot release calls");
      end if;

      --  The old version, in its own slot, ends with its own bank, which
      --  now holds only its own storage.

      declare
         Old_Identifier : constant Rose.Objects.Capability_Identifier :=
           Mem.Processes.Process_Identifier (R.Old_Pid);
      begin
         End_Process (R.Old_Pid, Bank_Gone => False);
         Remove_Instances (Old_Identifier);
      end;

      R.Committed := True;
      return True;
   end Commit_Replacement;

   -----------------
   -- Control_Cap --
   -----------------

   function Control_Cap
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid = 0 then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Mem.Processes.Get_Control_Cap (Pid);
   end Control_Cap;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is

      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));

   begin

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap,
         Delete_Cap      => Get_Cap (5));
      Rose.Trace.Set_Trace_Cap (Get_Cap (6));

      Console_Cap      := Get_Cap (1);
      Region_Count_Cap := Get_Cap (2);
      Region_Range_Cap := Get_Cap (3);
      Start_Paging_Cap := Get_Cap (4);

      Hold_Calls_Cap     := Get_Cap (7);
      Release_Calls_Cap  := Get_Cap (8);
      Swap_Processes_Cap := Get_Cap (9);
      Process_State_Cap  := Get_Cap (10);

      --  Nothing reads the cap set again (#826).

      Rose.System_Calls.Delete_Cap (Get_Cap_From_Set);

      Rose.Console_IO.Open (Console_Cap);

      Mem.Calls.Load_Memory_Map;

      Rose.Interfaces.Processes.Memory.Server.Create_Server
        (Server_Context => Server,
         New_Process    => New_Process'Access,
         Register_Process => Register_Process'Access,
         Page_Fault     => Page_Fault'Access,
         Take_Physical_Memory => Take_Physical_Memory'Access,
         Checkpoint => Checkpoint'Access,
         Set_Process_Banks => Set_Process_Banks'Access,
         Processes_Ended => Processes_Ended'Access,
         Set_Page_Log => Set_Page_Log'Access,
         Log_Committed => Log_Committed'Access);

      Rose.Interfaces.Processes.Process.Server.Attach_Interface
        (Server_Context => Server,
         Destroy => Destroy'Access,
         Add_Segment    => Add_Segment'Access,
         Add_Private_Segment => Add_Private_Segment'Access,
         Add_Nonpersistent_Segment => Add_Nonpersistent_Segment'Access,
         Remove_Segment => Remove_Segment'Access,
         Published_Interface => Published_Interface'Access,
         Get_Object_Id  => Mem.Processes.Get_Object_Id'Access,
         Exit_Process   => Exit_Process'Access,
         Heap_Interface => Heap_Interface'Access,
         Bank           => Process_Bank'Access,
         Control_Cap    => Control_Cap'Access,
         Publish_Interface => Publish_Interface'Access,
         Instanced      => True);

      Rose.Interfaces.Processes.Heap.Server.Attach_Interface
        (Server_Context    => Server,
         Current_Bound     => Current_Bound'Access,
         Request_New_Bound => Request_New_Bound'Access,
         Instanced         => True);

      Rose.Interfaces.Caps.Receiver.Server.Attach_Interface
        (Server_Context => Server,
         Send_Cap       => Send_Cap'Access,
         Instanced      => True);

      --  Attached last, so that the endpoints of the interfaces above
      --  are where they were.

      Rose.Interfaces.Checkpoints.Memory_Restore.Server.Attach_Interface
        (Server_Context       => Server,
         Restore_Process      => Mem.Processes.Restore_Process'Access,
         Restore_Segment      => Mem.Processes.Restore_Segment'Access,
         Restore_Zero_Segment => Mem.Processes.Restore_Zero_Segment'Access,
         Restore_Page         => Mem.Processes.Restore_Page'Access,
         Load_Page            => Mem.Processes.Load_Page'Access,
         Restore_Shared_Page  => Mem.Processes.Restore_Shared_Page'Access);

      --  Attached after Memory_Restore, for the same reason (#866).

      Rose.Interfaces.Processes.Replacer.Server.Attach_Interface
        (Server_Context  => Server,
         Replace_Process => Replace_Process'Access);

      Rose.Interfaces.Processes.Replacement.Server.Attach_Interface
        (Server_Context => Server,
         Release        => Release_Replacement'Access,
         Ready          => Replacement_Ready'Access,
         Bank           => Replacement_Bank'Access,
         Allow          => Allow_New_Version'Access,
         Commit         => Commit_Replacement'Access,
         Instanced      => True);

      --  Attached last, for the same reason (#99). The launcher's cap
      --  answers only these, so it can't change the process's segments
      --  or act as the process.

      Rose.Interfaces.Processes.Process_Control.Server.Attach_Interface
        (Server_Context      => Server,
         Published_Interface => Published_Interface'Access,
         Destroy             => Destroy'Access,
         Get_Object_Id       => Mem.Processes.Get_Object_Id'Access,
         Instanced           => True);

   end Create_Server;

   ----------------------
   -- Current_Heap_Top --
   ----------------------

   function Current_Bound
     (Id : Rose.Objects.Capability_Identifier)
     return Rose.Words.Word
   is
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid = 0 then
         return 0;
      end if;

      return Rose.Words.Word
        (Mem.Processes.Get_Process_Heap_Bound (Process => Pid));
   end Current_Bound;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : Rose.Objects.Capability_Identifier) is
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid /= 0 then
         End_Process (Pid, Bank_Gone => False);
      end if;
      Remove_Instances (Id);
   end Destroy;

   -----------------
   -- Drain_Ended --
   -----------------

   procedure Drain_Ended is
      use type Rose.Objects.Object_Id;
   begin
      --  The store has destroyed the banks of these processes, so they
      --  end, and their caps stop working.

      loop
         declare
            Oid : constant Rose.Objects.Object_Id :=
                    Mem.Processes.Take_Ended;
            Pid : Process_Id;
         begin
            exit when Oid = 0;
            Pid := Mem.Processes.Find_By_Oid (Oid);
            if Pid /= 0 then
               declare
                  Identifier : constant Rose.Objects.Capability_Identifier :=
                                 Mem.Processes.Process_Identifier (Pid);
               begin
                  End_Process (Pid, Bank_Gone => True);
                  Remove_Instances (Identifier);
               end;
            end if;
         end;
      end loop;
   end Drain_Ended;

   -----------------
   -- End_Process --
   -----------------

   procedure End_Process
     (Pid       : Process_Id;
      Bank_Gone : Boolean)
   is
      Oid : constant Rose.Objects.Object_Id :=
              Mem.Processes.Process_Object_Id (Pid);
   begin
      Mem.Processes.Release_Shared_Segments (Pid);
      Mem.Page_Table.Delete_All (Pid);

      --  Its pages paged out since the last checkpoint are not added to
      --  any index (ADR 0028, section 4).

      Mem.Change_Table.Forget (Pid);
      Mem.Processes.Kill_Process (Pid);
      if not Bank_Gone then
         Mem.Processes.End_Bank (Oid);

         --  The store queued the processes below, and sent to
         --  Processes_Ended while mem was waiting for it, which mem
         --  doesn't see then.  So mem ends them now.

         Drain_Ended;
      end if;
   end End_Process;

   ---------------------
   -- End_Replacement --
   ---------------------

   procedure End_Replacement is
      R : Replacement_Record renames Current_Replacement;
   begin
      Rose.System_Calls.Delete_Cap (R.Old_Cap);
      Rose.System_Calls.Delete_Cap (R.New_Cap);
      Rose.System_Calls.Delete_Cap (R.Replacement_Cap);
      R := (others => <>);
   end End_Replacement;

   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Exit_Status : Natural)
   is
      pragma Unreferenced (Exit_Status);
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid /= 0 then
         End_Process (Pid, Bank_Gone => False);
      end if;
      Remove_Instances (Id);
   end Exit_Process;

   ------------------
   -- Handle_Fault --
   ------------------

   procedure Handle_Fault
     (Object   : Rose.Objects.Object_Id;
      Virtual  : Rose.Words.Word;
      Physical : Rose.Words.Word;
      Action   : Rose.Interfaces.Processes.Memory.Page_Access_Type;
      May_Wait : Boolean)
   is
      use type Rose.Capabilities.Capability;
      use Rose.Addresses;
      use all type Rose.Interfaces.Processes.Memory.Page_Access_Type;
      Absent : Boolean;
      --  No page is mapped there, as far as mem knows.
      Valid,
      Readable,
      Writable,
      Executable,
      Persistent : Boolean := False;
      Virtual_Page : constant Virtual_Page_Address :=
                       Virtual_Page_Address (Virtual);
      Physical_Page : constant Physical_Page_Address :=
                        Physical_Page_Address (Physical);
      Pid    : constant Process_Id :=
                        Mem.Processes.Get_Process_Id (Object);
      Page_Object : Rose.Objects.Object_Id;
      Start  : constant Natural := Rose.Trace.Ticks;
      use type Rose.Objects.Object_Id;
      --  When the fault arrived, for timing it (#195).
   begin

      if not Mem.Processes.Is_Valid_Process_Id (Pid) then
         Protection_Fault (Object, Virtual_Page, "invalid process id");
         return;
      end if;

      Mem.Processes.Get_Process_Segment
        (Pid, Virtual_Page, Page_Object,
         Valid, Readable, Writable, Executable, Persistent);

      if not Valid then
         Mem.Processes.Count_Fault (Pid, Mem.Processes.Invalid_Fault, 0);
         Protection_Fault (Object, Virtual_Page, "invalid page reference");
         return;
      end if;

      case Action is
         when Read =>
            if not Readable then
               Protection_Fault (Object, Virtual_Page,
                                 "attempt to read non-readable page");
               return;
            end if;
         when Write =>
            if not Writable then
               Protection_Fault (Object, Virtual_Page,
                                 "attempt to write non-writable page");
               return;
            end if;
         when Execute =>
            if not Executable then
               Protection_Fault (Object, Virtual_Page,
                                 "attempt to execute non-executable page");
               return;
            end if;
      end case;

      --  A page can be dropped while a write fault on it waits here,
      --  for a fault from another process may need its frame (#973).
      --  The kernel reported a frame, but it has gone, so the page is
      --  brought back as if it had never been mapped.

      Absent := Physical_Page = 0
        or else not Mem.Page_Table.Has_Element
          (Mem.Page_Table.Find (Pid, Virtual_Page));

      if Absent
        and then Mem.Processes.Shared_Page (Pid, Virtual_Page) /= 0
      then
         Shared_Page_Fault
           (Object       => Object,
            Pid          => Pid,
            Virtual_Page => Virtual_Page,
            Store_Page   => Mem.Processes.Shared_Page (Pid, Virtual_Page),
            Readable     => Readable,
            Writable     => Writable,
            Executable   => Executable,
            Start        => Start);
      elsif Absent then
         declare
            Shared     : constant Boolean := Persistent and then not Writable;
            --  Pages of read-only segments are cached, and shared by the
            --  processes that map them (#200).
            Store_Page : constant Rose.Objects.Object_Id :=
                           (if Shared
                            then Mem.Processes.Region_Page (Pid, Virtual_Page)
                            else 0);
            Cached     : constant Physical_Page_Address :=
                           (if Store_Page /= 0
                            then Mem.Page_Cache.Find (Store_Page)
                            else 0);
            Have_Page  : Boolean;
            Page       : Rose.Addresses.Physical_Page_Address;
            From_Log   : Boolean;
         begin
            if Cached /= 0 then
               Mem.Virtual_Map.Map
                 (Process       => Object,
                  Virtual_Page  => Virtual_Page,
                  Physical_Page => Cached,
                  Readable      => Readable,
                  Writable      => Writable,
                  Executable    => Executable,
                  Persistent    => Persistent);
               Mem.Page_Cache.Acquire (Cached);
               Mem.Processes.Resume_Process (Pid);
               Mem.Processes.Count_Fault
                 (Pid, Mem.Processes.Cached_Fault, Rose.Trace.Ticks - Start);
               return;
            end if;

            Mem.Virtual_Map.Allocate_Page (Page, Have_Page);

            if not Have_Page
              and then May_Wait
              and then Request_Checkpoint_Cap /= 0
              and then Held_Count < Held_Faults'Last
              and then
                (Checkpoint_Requested
                 or else Mem.Change_Table.Count > 0
                 or else Mem.Page_Table.Dirty_Page_Count > 0)
            then

               --  A checkpoint makes the changed pages clean, and empties
               --  the table of changes, so pages can be freed again. The
               --  process waits for it (ADR 0028, section 6).

               Held_Count := Held_Count + 1;
               Held_Faults (Held_Count) := (Object, Virtual, Action);

            elsif not Have_Page then
               Mem.Processes.Fault_Process (Pid, "out of memory");
            else

               Mem.Processes.Initialize_Page
                 (Process       => Pid,
                  Physical_Page => Page,
                  Virtual_Page  => Virtual_Page,
                  From_Log      => From_Log);

               Mem.Virtual_Map.Map
                 (Process       => Object,
                  Virtual_Page  => Virtual_Page,
                  Physical_Page => Page,
                  Readable      => Readable,
                  Writable      => Writable,
                  Executable    => Executable,
                  Persistent    => Persistent);

               if Store_Page /= 0 then
                  Mem.Page_Cache.Add (Store_Page, Page);
               end if;

               Mem.Processes.Resume_Process (Pid);
               Mem.Processes.Count_Fault
                 (Pid,
                  (if From_Log then Mem.Processes.Log_Fault
                   elsif Persistent then Mem.Processes.Region_Fault
                   else Mem.Processes.Zero_Fault),
                  Rose.Trace.Ticks - Start);

            end if;
         end;
      else
         declare
            Position : constant Mem.Page_Table.Cursor :=
              Mem.Page_Table.Find (Pid, Virtual_Page);
         begin
            if not Mem.Page_Table.Has_Element (Position) then
               Protection_Fault (Object, Virtual_Page,
                                 "virtual address not found");
            else

               case Action is
                  when Read =>
                     Mem.Processes.Fault_Process (Pid, "read not allowed");
                  when Execute =>
                     Mem.Processes.Fault_Process (Pid, "execute not allowed");
                  when Write =>
                     if Mem.Page_Table.Is_Writable (Position) then
                        --  page was already writable
                        Rose.Console_IO.Put_Line
                          ("mem: second fault on writable page");
                        Rose.Console_IO.Put (Object);
                        Rose.Console_IO.New_Line;

                        Rose.Console_IO.Put ("mem: page fault: pid=");
                        Rose.Console_IO.Put (Natural (Pid));
                        Rose.Console_IO.Put ("; virtual=");
                        Rose.Console_IO.Put (Virtual);
                        Rose.Console_IO.Put ("; physical=");
                        Rose.Console_IO.Put (Physical);
                        Rose.Console_IO.Put ("; action=");
                        Rose.Console_IO.Put (case Action is
                                                when Read    => "read",
                                                when Write   => "write",
                                                when Execute => "execute");
                        Rose.Console_IO.New_Line;

                        Mem.Processes.Fault_Process
                          (Pid, "second fault on writable page");
                     else
                        --  page should be made writable, and dirty
                        --  flag should be set (since this is the
                        --  first write)
                        Mem.Page_Table.Set_Writable (Position);
                        Mem.Shared_Pages.Set_Modified (Physical_Page);
                        Mem.Processes.Resume_Process (Pid);
                        Mem.Processes.Count_Fault
                          (Pid, Mem.Processes.Write_Fault,
                           Rose.Trace.Ticks - Start);
                     end if;
               end case;
            end if;
         end;
      end if;

   end Handle_Fault;

   --------------------
   -- Heap_Interface --
   --------------------

   function Heap_Interface
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid = 0 then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Mem.Processes.Get_Heap_Cap (Pid);
   end Heap_Interface;

   ---------------------
   -- Kernel_Activity --
   ---------------------

   procedure Kernel_Activity
     (Cap      : Rose.Capabilities.Capability;
      Activity : out Rose.Words.Word;
      Oid      : out Rose.Objects.Object_Id)
   is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Activity := 0;
      Oid := 0;

      if Process_State_Cap = 0 or else Cap = 0 then
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Process_State_Cap);
      Rose.System_Calls.Send_Cap (Params, Cap);
      Rose.System_Calls.Receive_Words
        (Params, 1 + Rose.Invocation.Object_Id_Words);
      Rose.System_Calls.Invoke_Capability (Params);

      if not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
        and then Natural (Params.Control.Last_Sent_Word)
          >= Rose.Invocation.Object_Id_Words
      then
         Activity := Params.Data (0);
         Oid := Rose.Invocation.Get_Object_Id (Params, 1);
      end if;
   end Kernel_Activity;

   ---------------------
   -- Kernel_Handover --
   ---------------------

   function Kernel_Handover
     (Op     : Rose.Capabilities.Capability;
      First  : Rose.Capabilities.Capability;
      Second : Rose.Capabilities.Capability := 0)
      return Boolean
   is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Op = 0 or else First = 0 then
         return False;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Op);
      Rose.System_Calls.Send_Cap (Params, First);
      if Second /= 0 then
         Rose.System_Calls.Send_Cap (Params, Second);
      end if;
      Rose.System_Calls.Invoke_Capability (Params);
      return not Params.Control.Flags (Rose.Invocation.Error);
   end Kernel_Handover;

   -------------------
   -- Log_Committed --
   -------------------

   procedure Log_Committed (Id : Rose.Objects.Capability_Identifier) is
      pragma Unreferenced (Id);
   begin
      --  The log takes batches again, even if it refused one between
      --  mem's part of the checkpoint and the commit.

      Mem.Virtual_Map.End_Checkpoint;
      Mem.Cleaning.Clean;
      Ask_For_Checkpoint;
   end Log_Committed;

   -----------------
   -- New_Process --
   -----------------

   function New_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Process     : Rose.Capabilities.Capability;
      Bank        : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Mem.Processes.New_Process (Process, Bank);
   end New_Process;

   ----------------
   -- Page_Fault --
   ----------------

   procedure Page_Fault
     (Id       : Rose.Objects.Capability_Identifier;
      Object   : Rose.Objects.Object_Id;
      Virtual  : Rose.Words.Word;
      Physical : Rose.Words.Word;
      Action   : Rose.Interfaces.Processes.Memory.Page_Access_Type)
   is
      pragma Unreferenced (Id);
   begin
      Handle_Fault (Object, Virtual, Physical, Action, May_Wait => True);
      Ask_For_Checkpoint;
   end Page_Fault;

   ------------------
   -- Process_Bank --
   ------------------

   function Process_Bank
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
   begin
      return Mem.Processes.Get_Bank_Cap (Mem.Processes.Find_Process (Id));
   end Process_Bank;

   ---------------------
   -- Processes_Ended --
   ---------------------

   procedure Processes_Ended (Id : Rose.Objects.Capability_Identifier) is
      pragma Unreferenced (Id);
   begin
      Drain_Ended;
   end Processes_Ended;

   ----------------------
   -- Protection_Fault --
   ----------------------

   procedure Protection_Fault
     (Process : Rose.Objects.Object_Id;
      Address : Rose.Addresses.Virtual_Page_Address;
      Message : String)
   is
      use Rose.Words;
   begin
      Rose.Console_IO.New_Line;
      Rose.Console_IO.Put_Line ("*** mem: protection fault ***");
      Rose.Console_IO.Put ("mem: error pid ");
      Rose.Console_IO.Put (Rose.Words.Word_64 (Process));
      Rose.Console_IO.Put ("; page ");
      Rose.Console_IO.Put (Word (Address) * 4096);
      Rose.Console_IO.Put (": ");
      Rose.Console_IO.Put (Message);
      Rose.Console_IO.New_Line;
      Mem.Processes.Fault_Process
        (Mem.Processes.Get_Process_Id (Process), "protection fault");
   end Protection_Fault;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Id            : Rose.Objects.Capability_Identifier;
      Interface_Cap : Rose.Capabilities.Capability)
   is
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid = 0 then
         return;
      end if;

      Mem.Processes.Set_Published_Interface_Cap (Pid, Interface_Cap);
      Rose.Server.Unblock_Endpoint
        (Server, Rose.Interfaces.Processes.Process
          .Published_Interface_Endpoint);
   end Publish_Interface;

   -------------------------
   -- Published_Interface --
   -------------------------

   function Published_Interface
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid = 0 then
         Rose.Console_IO.Put_Line
           ("mem: published interface: no such process");
         return 0;
      end if;

      return Cap : constant Rose.Capabilities.Capability :=
        Mem.Processes.Published_Interface_Cap (Pid)
      do
         if Cap = Rose.Capabilities.Null_Capability then
            Rose.Server.Block_Current_Request (Server);
         end if;
      end return;
   end Published_Interface;

   ----------------------
   -- Register_Process --
   ----------------------

   procedure Register_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Process     : Rose.Capabilities.Capability;
      Exec_Base   : Rose.Words.Word;
      Exec_Bound  : Rose.Words.Word;
      Text_Base   : Rose.Words.Word;
      Text_Bound  : Rose.Words.Word;
      Data_Base   : Rose.Words.Word;
      Data_Bound  : Rose.Words.Word;
      Stack_Base  : Rose.Words.Word;
      Stack_Bound : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Pid : constant Process_Id :=
                     Mem.Processes.Register_Process
                       (Process, Boot_Module => True);
      function A
        (Value : Rose.Words.Word)
         return Rose.Addresses.Virtual_Page_Address
      is (Rose.Addresses.Virtual_Page_Address (Value));

      Added     : Boolean;
      All_Added : Boolean := True;
      --  mem lays out a boot module's segments itself, so one that is
      --  left out is a bug in mem, and there is no caller to fail.

   begin
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Exec_Base),
         Virtual_Bound => A (Exec_Bound),
         Readable      => True,
         Writable      => False,
         Executable    => True,
         Resizable     => False,
         Success       => Added);
      All_Added := All_Added and then Added;
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Text_Base),
         Virtual_Bound => A (Text_Bound),
         Readable      => True,
         Writable      => False,
         Executable    => False,
         Resizable     => False,
         Success       => Added);
      All_Added := All_Added and then Added;
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Data_Base),
         Virtual_Bound => A (Data_Bound),
         Readable      => True,
         Writable      => True,
         Executable    => False,
         Resizable     => False,
         Success       => Added);
      All_Added := All_Added and then Added;
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Data_Bound),
         Virtual_Bound => A (Data_Bound),
         Readable      => True,
         Writable      => True,
         Executable    => False,
         Resizable     => True,
         Success       => Added);
      All_Added := All_Added and then Added;
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Stack_Base),
         Virtual_Bound => A (Stack_Bound),
         Readable      => True,
         Writable      => True,
         Executable    => False,
         Resizable     => False,
         Success       => Added);
      All_Added := All_Added and then Added;

      if not All_Added then
         Rose.Console_IO.Put_Line
           ("mem: register boot module: a segment was left out");
      end if;
   end Register_Process;

   -------------------------
   -- Release_Replacement --
   -------------------------

   procedure Release_Replacement
     (Id : Rose.Objects.Capability_Identifier)
   is
      use type Rose.Objects.Object_Id;
      R : Replacement_Record renames Current_Replacement;
   begin
      if not Is_Current (Id) then
         return;
      end if;

      --  A replacement that didn't commit fails: the new version ends,
      --  with its fresh bank, and the old one gets its calls (section 7).
      --  The old version takes back its deferred calls' reply caps when
      --  it next serves a call (#879).

      if not R.Committed then
         if R.New_Oid /= 0 then
            declare
               New_Pid : constant Process_Id :=
                 Mem.Processes.Get_Process_Id (R.New_Oid);
            begin
               if New_Pid /= 0 then
                  declare
                     New_Identifier : constant
                       Rose.Objects.Capability_Identifier :=
                         Mem.Processes.Process_Identifier (New_Pid);
                  begin
                     End_Process (New_Pid, Bank_Gone => False);
                     Remove_Instances (New_Identifier);
                  end;
               end if;
            end;
         end if;

         if not Kernel_Handover (Release_Calls_Cap, R.Old_Cap) then
            Rose.Console_IO.Put_Line
              ("mem: replacement: cannot release calls");
         end if;
      end if;

      End_Replacement;
   end Release_Replacement;

   ----------------------
   -- Remove_Instances --
   ----------------------

   procedure Remove_Instances (Id : Rose.Objects.Capability_Identifier) is
   begin
      Rose.Interfaces.Processes.Process.Server.Remove_Instance (Id);
      Rose.Interfaces.Processes.Process_Control.Server.Remove_Instance (Id);
      Rose.Interfaces.Processes.Heap.Server.Remove_Instance (Id);
      Rose.Interfaces.Caps.Receiver.Server.Remove_Instance (Id);
   end Remove_Instances;

   --------------------
   -- Remove_Segment --
   --------------------

   procedure Remove_Segment
     (Id           : Rose.Objects.Capability_Identifier;
      Virtual_Base : Rose.Words.Word)
   is
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid = 0 then
         return;
      end if;

      Mem.Processes.Remove_Segment
        (Process      => Pid,
         Virtual_Base => Rose.Addresses.Virtual_Page_Address (Virtual_Base));
   end Remove_Segment;

   ---------------------
   -- Replace_Process --
   ---------------------

   function Replace_Process
     (Id  : Rose.Objects.Capability_Identifier;
      Old : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      use type Rose.Objects.Object_Id;
      use type Rose.Objects.Capability_Identifier;
      Activity : Rose.Words.Word;
      Oid      : Rose.Objects.Object_Id;
      Pid      : Process_Id;
   begin
      if Current_Replacement.Active then
         Rose.System_Calls.Delete_Cap (Old);
         return 0;
      end if;

      Kernel_Activity (Old, Activity, Oid);
      Pid := (if Oid = 0 then 0 else Mem.Processes.Get_Process_Id (Oid));

      --  Holding fails during a checkpoint (ADR 0032, section 5).

      if not Mem.Processes.Is_Launched (Pid)
        or else not Kernel_Handover (Hold_Calls_Cap, Old)
      then
         Rose.System_Calls.Delete_Cap (Old);
         return 0;
      end if;

      Last_Replacement_Id := Last_Replacement_Id + 1;
      Current_Replacement :=
        (Active          => True,
         Committed       => False,
         Identifier      => Last_Replacement_Id,
         Replacement_Cap =>
           Rose.System_Calls.Server.Create_Endpoint
             (Create_Cap  => Create_Endpoint_Cap,
              Endpoint_Id =>
                Rose.Interfaces.Processes.Replacement.Replacement_Interface,
              Identifier  => Last_Replacement_Id),
         Old_Cap         => Old,
         Old_Pid         => Pid,
         New_Cap         => 0,
         New_Oid         => 0);

      return Current_Replacement.Replacement_Cap;
   end Replace_Process;

   ----------------------
   -- Replacement_Bank --
   ----------------------

   function Replacement_Bank
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
   begin
      if not Is_Current (Id) then
         return 0;
      end if;
      return Mem.Processes.Parent_Bank (Current_Replacement.Old_Pid);
   end Replacement_Bank;

   -----------------------
   -- Replacement_Ready --
   -----------------------

   function Replacement_Ready
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean
   is
      use type Rose.Words.Word;
      Activity : Rose.Words.Word;
      Unused   : Rose.Objects.Object_Id;
   begin
      if not Is_Current (Id) or else Current_Replacement.Committed then
         return False;
      end if;
      Kernel_Activity (Current_Replacement.Old_Cap, Activity, Unused);
      return Activity = 1;
   end Replacement_Ready;

   ------------------------
   -- Replay_Held_Faults --
   ------------------------

   procedure Replay_Held_Faults is
   begin
      --  A fault that still finds no page fails its process now, rather
      --  than wait for another checkpoint, so nothing is added to the
      --  held faults while they are handled.

      for Fault of Held_Faults (1 .. Held_Count) loop
         Handle_Fault
           (Fault.Object, Fault.Virtual, 0, Fault.Action,
            May_Wait => False);
      end loop;
      Held_Count := 0;
   end Replay_Held_Faults;

   ----------------------
   -- Request_Heap_Top --
   ----------------------

   procedure Request_New_Bound
     (Id        : Rose.Objects.Capability_Identifier;
      New_Bound : Rose.Words.Word)
   is
      Pid : constant Process_Id := Mem.Processes.Find_Process (Id);
   begin
      if Pid = 0 then
         return;
      end if;

      Mem.Processes.Resize_Segment
        (Process      => Pid,
         New_Virtual_Bound =>
           Rose.Addresses.Virtual_Page_Address (New_Bound));
   end Request_New_Bound;

   ------------------
   -- Set_Page_Log --
   ------------------

   procedure Set_Page_Log
     (Id  : Rose.Objects.Capability_Identifier;
      Log : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
   begin
      Mem.Page_Log.Set_Cap (Log);
   end Set_Page_Log;

   -----------------------
   -- Set_Process_Banks --
   -----------------------

   procedure Set_Process_Banks
     (Id    : Rose.Objects.Capability_Identifier;
      Banks : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
   begin
      Mem.Processes.Set_Process_Banks (Banks);

      --  The store tells mem of processes to end by sending to
      --  Processes_Ended, without waiting.

      Mem.Processes.Set_Ended
        (Rose.Interfaces.Processes.Memory.Server.Get_Processes_Ended_Cap);
   end Set_Process_Banks;

   -----------------------
   -- Shared_Page_Fault --
   -----------------------

   procedure Shared_Page_Fault
     (Object        : Rose.Objects.Object_Id;
      Pid           : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      Store_Page    : Rose.Objects.Object_Id;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Start         : Natural)
   is
      use type Rose.Addresses.Physical_Page_Address;
      Found     : constant Rose.Addresses.Physical_Page_Address :=
                    Mem.Shared_Pages.Find (Store_Page);
      Page      : Rose.Addresses.Physical_Page_Address;
      Have_Page : Boolean;
      Added     : Boolean;
      From_Log  : Boolean;
      pragma Warnings (Off, From_Log);
      --  A shared page is never read from the log.
   begin
      if Found /= 0 then
         Mem.Shared_Pages.Acquire (Found);
         Page := Found;
      else
         Mem.Virtual_Map.Allocate_Page (Page, Have_Page);

         if not Have_Page then
            Mem.Processes.Fault_Process (Pid, "out of memory");
            return;
         end if;

         Mem.Shared_Pages.Add (Store_Page, Page, Added);
         if not Added then
            Mem.Physical_Map.Deallocate_Page (Page);
            Mem.Processes.Fault_Process (Pid, "too many shared pages");
            return;
         end if;

         Mem.Processes.Initialize_Page
           (Process       => Pid,
            Physical_Page => Page,
            Virtual_Page  => Virtual_Page,
            From_Log      => From_Log);
      end if;

      Mem.Virtual_Map.Map
        (Process       => Object,
         Virtual_Page  => Virtual_Page,
         Physical_Page => Page,
         Readable      => Readable,
         Writable      => Writable,
         Executable    => Executable,
         Persistent    => True);

      Mem.Processes.Resume_Process (Pid);
      Mem.Processes.Count_Fault
        (Pid,
         (if Found /= 0 then Mem.Processes.Cached_Fault
          else Mem.Processes.Region_Fault),
         Rose.Trace.Ticks - Start);
   end Shared_Page_Fault;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Start_Paging_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
      Rose.Server.Start_Server (Server);
   end Start_Server;

   --------------------------
   -- Take_Physical_Memory --
   --------------------------

   procedure Take_Physical_Memory
     (Id     : in     Rose.Objects.Capability_Identifier;
      Size   : in     Rose.Words.Word;
      Start  :    out Rose.Words.Word;
      Amount :    out Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Taken_Start  : Rose.Addresses.Physical_Address;
      Taken_Amount : Rose.Addresses.Physical_Bytes;
   begin

      --  The free pages are what the kernel's heap can have, so first make
      --  sure there are some, as a page fault would (#973). They may be
      --  scattered, so the kernel is given the longest run there is, if
      --  there is none as long as it asked for (#1051). It asks again
      --  when it needs more.

      Mem.Virtual_Map.Keep_Reserve;
      Mem.Physical_Map.Take_Memory
        (Rose.Addresses.Physical_Bytes (Size), Taken_Start, Taken_Amount);

      --  The kernel adds nothing to its heap for an amount of zero, and
      --  asks no more until a checkpoint has ended. Nothing is written
      --  to the console here: this can happen while mem is paging, and
      --  the console can wait on mem (#971).

      Start := Rose.Words.Word (Taken_Start);
      Amount := Rose.Words.Word (Taken_Amount);
   end Take_Physical_Memory;

end Mem.Server;
