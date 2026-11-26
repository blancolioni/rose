with Rose.Console_IO;
with Rose.Server;
with Rose.Invocation;
with Rose.Trace;
with Rose.System_Calls.Server;
with Rose.Limits;

with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Storage.Region.Client;
with Rose.Interfaces.Storage.Region_Access.Client;
with Rose.Interfaces.Processes.Heap;
with Rose.Interfaces.Processes.Process_Control;
with Rose.Interfaces.Processes.Process;
with Rose.Interfaces.Storage.Process_Banks.Client;
with Rose.Interfaces.Processes.Kernel_Process.Client;

with Mem.Calls;

with Mem.Log_Index;
with Mem.Page_Cache;
with Mem.Page_Log;
with Mem.Page_Table;
with Mem.Physical_Map;
with Mem.Shared_Pages;
with Mem.Virtual_Map;

package body Mem.Processes is

   Log_Segments : constant Boolean := False;

   type Process_State is (Available, Active, Faulted);

   type Segment_Flag is
     (Read, Write, Execute, Extensible, Persistent, Shared);

   type Segment_Flag_Array is array (Segment_Flag) of Boolean;

   subtype Page_Buffer is
     System.Storage_Elements.Storage_Array (1 .. 4096);
   Long_Buffer : System.Storage_Elements.Storage_Array (1 .. 8192);

   Max_Segments : constant := Rose.Checkpoints.Max_Saved_Segments;
   --  32: ADR 0025 has what a larger limit would cost.
   type Segment_Count is range 0 .. Max_Segments;
   subtype Segment_Index is Segment_Count range 1 .. Segment_Count'Last;

   type Segment_Record is
      record
         Base          : Rose.Addresses.Virtual_Page_Address := 0;
         Bound         : Rose.Addresses.Virtual_Page_Address := 0;
         Limit         : Rose.Addresses.Virtual_Page_Address := 0;
         Region        : Rose.Interfaces.Storage.Region_Access.Client
                           .Region_Access_Client;
         Owner         : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
         Region_Base   : Rose.Objects.Object_Id := 0;
         Region_Bound  : Rose.Objects.Object_Id := 0;
         Region_Offset : Rose.Objects.Object_Id := 0;
         Flags         : Segment_Flag_Array := (others => False);
         Private_Region : Boolean := False;
      end record;
   --  Private_Region is True if the region belongs to the process, and is
   --  released when it is gone. Pages are read and written through
   --  Region, a Region_Access, which can't release the region. Owner is
   --  the cap of a private segment's Region, which mem opens only to
   --  release it, so that a segment holds one client (#403).

   type Segment_Record_Array is array (Segment_Index) of Segment_Record;

   procedure Open_Region
     (Segment : in out Segment_Record;
      Cap     : Rose.Capabilities.Capability);
   --  Open the segment's Region from Cap, which is a Region_Access, or
   --  for a private segment the Region itself, which is kept as Owner.

   procedure Get_Range
     (Segment : Segment_Record;
      Base    : out Rose.Objects.Object_Id;
      Bound   : out Rose.Objects.Object_Id;
      Success : out Boolean);

   function Interface_Cap_Of
     (Bank : Rose.Interfaces.Storage.Bank.Client.Bank_Client)
      return Rose.Capabilities.Capability;
   --  The interface cap of a Bank client that a call returned, opened. Its
   --  operation caps are deleted: mem keeps only the interface cap, and
   --  the others would stay in its table for ever (#1131).

   procedure Keep_Used_Caps
     (Region : in out Rose.Interfaces.Storage.Region_Access.Client
                .Region_Access_Client;
      Shared : Boolean);
   --  Delete the caps of an opened segment's Region that mem never calls:
   --  it keeps Get, and Put for a Shared segment, whose pages it writes
   --  back. Get_Range is called once, before this. Each segment held five
   --  caps, and mem's table limited the processes (#1131).

   subtype Kernel_Process_Client is
     Rose.Interfaces.Processes.Kernel_Process.Client.Kernel_Process_Client;

   Launch_Reserve : constant := Rose.Limits.Max_Processes + 64;
   --  Mem refuses a launch when it can create fewer caps than this. Each
   --  call to mem needs a reply cap in its table, and a process it can't
   --  reply to faults: the reserve leaves one for each process that may
   --  be waiting on mem, and room for the caps of one more launch, about
   --  30 (#1131).

   function Free_Caps return Natural;
   --  How many caps mem can still create, from the kernel. Natural'Last
   --  if mem doesn't have the kernel's cap.

   procedure Keep_Used_Caps (Process : in out Kernel_Process_Client);
   --  Delete the caps of an opened Kernel_Process that mem never calls
   --  after Get_Object_Id: it keeps Destroy, Resume and Fault (#1131).

   type Fault_Count is
      record
         Count : Natural := 0;
         Ticks : Natural := 0;
      end record;

   type Fault_Count_Array is array (Fault_Kind) of Fault_Count;

   Store_Reads : Natural := 0;
   --  Pages read from a region, by all processes since boot (#210).

   type Memory_Process_Record is
      record
         State        : Process_State := Available;
         Oid          : Rose.Objects.Object_Id;
         Segments     : Segment_Record_Array;
         Num_Segments : Segment_Count;
         Process      : Kernel_Process_Client;
         Heap         : Rose.Capabilities.Capability;
         Server       : Rose.Capabilities.Capability;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Control_Cap   : Rose.Capabilities.Capability := 0;
         Bank_Cap      : Rose.Capabilities.Capability := 0;
         Faults        : Fault_Count_Array;
         Transient     : Boolean := False;
      end record;
   --  A Transient process is never saved (#524).
   --  Bank_Cap is the cap for the process's own bank (#750), which mem
   --  asks the store for when it is first needed, and keeps.

   No_Process : constant Memory_Process_Record :=
                  (State        => Available,
                   Oid          => 0,
                   Num_Segments => 0,
                   Heap         => 0,
                   Server       => 0,
                   others       => <>);
   --  An entry with no process, which a new one starts from: a copy, not
   --  an aggregate, which would be built on mem's one-page stack (#974,
   --  #1004).

   subtype Real_Process_Id is
     Process_Id range 1 .. Rose.Limits.Mem_Process_Entries;

   type Memory_Process_Table is
     array (Real_Process_Id) of aliased Memory_Process_Record;

   Process_Table : Memory_Process_Table;

   Process_Banks : Rose.Interfaces.Storage.Process_Banks.Client
     .Process_Banks_Client;
   Have_Banks    : Boolean := False;
   --  The storage server's record of each process's own bank (#750), from
   --  Set_Process_Banks.

   function Identifier_Of
     (Oid : Rose.Objects.Object_Id)
      return Rose.Objects.Capability_Identifier
      renames Rose.Objects.Process_Identifier;
   --  The identifier of the caps for the process with object id Oid. The
   --  kernel uses it too, when the process exits (#697).

   Last_Boot_Pid : constant Real_Process_Id :=
                     Rose.Limits.Mem_Boot_Entries;
   --  Entries 1 to Last_Boot_Pid are for boot modules, and the rest for
   --  launched processes (Register_Process, #873), as many as the kernel
   --  can run (#1090).

   Next_Boot_Pid     : Real_Process_Id := 1;
   Next_Launched_Pid : Real_Process_Id := Last_Boot_Pid + 1;

   procedure Report_Segment (Segment : Segment_Record);

   procedure Add_Restored_Segment
     (Mem_Pid : Natural;
      Base    : Rose.Words.Word;
      Bound   : Rose.Words.Word;
      Limit   : Rose.Words.Word;
      Flags   : Rose.Words.Word;
      Success : out Boolean);
   --  Add a segment saved in a checkpoint to process Mem_Pid, without a
   --  region. Success is False, and the request fails, if the process
   --  isn't valid, has too many segments, or the segment overlaps one it
   --  has (#970).

   procedure Report_Overlap
     (Process : Process_Id;
      Base    : Rose.Addresses.Virtual_Page_Address;
      Bound   : Rose.Addresses.Virtual_Page_Address);
   --  Say that the segment from Base to Bound overlaps one of Process's,
   --  and which (#970).

   function Overlaps
     (P     : Memory_Process_Record;
      Base  : Rose.Addresses.Virtual_Page_Address;
      Bound : Rose.Addresses.Virtual_Page_Address)
      return Boolean;
   --  True if any page from Base to Bound - 1 is in a segment of P, or
   --  in the range a resizable segment of P can grow into.

   procedure Close_Segment (Segment : in out Segment_Record);
   --  Release the segment's region, if it belongs to the process, and
   --  close it.

   procedure New_Restored_Page
     (Page      : System.Storage_Elements.Storage_Array;
      Physical  : out Rose.Addresses.Physical_Page_Address;
      Success   : out Boolean);
   --  Allocate a physical page and copy Page, saved in a checkpoint, into
   --  it. Success is False if there is no memory for it.

   procedure Unmap_Segment_Pages
     (Process : Process_Id;
      Segment : Segment_Record;
      Unmap   : Boolean);
   --  Forget the pages of Process in Segment, and free them, writing back
   --  a shared page that nothing else maps if it has changed. If Unmap is
   --  True, also remove them from the process's page table, which isn't
   --  needed when the process is gone.

   -------------------------------
   -- Add_Nonpersistent_Segment --
   -------------------------------

   procedure Add_Nonpersistent_Segment
     (Process        : Process_Id;
      Virtual_Base   : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound  : Rose.Addresses.Virtual_Page_Address;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      Resizable      : Boolean;
      Success        : out Boolean)
   is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Success := False;

      if P.Num_Segments = Max_Segments then
         Rose.Console_IO.Put
           ("mem: add segment: too many segments, process ");
         Rose.Console_IO.Put (Natural (Process));
         Rose.Console_IO.New_Line;
         return;
      end if;

      if Overlaps (P, Virtual_Base, Virtual_Bound) then
         Report_Overlap (Process, Virtual_Base, Virtual_Bound);
         return;
      end if;

      P.Num_Segments := P.Num_Segments + 1;
      P.Segments (P.Num_Segments) := Segment_Record'
        (Base         => Virtual_Base,
         Bound        => (if Resizable then Virtual_Base else Virtual_Bound),
         Limit        => Virtual_Bound,
         Region       => <>,
         Owner        => Rose.Capabilities.Null_Capability,
         Region_Base  => 0,
         Region_Bound => 0,
         Region_Offset => 0,
         Flags        =>
           (Read       => Readable,
            Write      => Writable,
            Execute    => Executable,
            Extensible => Resizable,
            Persistent => False,
            Shared     => False),
         Private_Region => False);
      Success := True;

   end Add_Nonpersistent_Segment;

   --------------------------
   -- Add_Restored_Segment --
   --------------------------

   procedure Add_Restored_Segment
     (Mem_Pid : Natural;
      Base    : Rose.Words.Word;
      Bound   : Rose.Words.Word;
      Limit   : Rose.Words.Word;
      Flags   : Rose.Words.Word;
      Success : out Boolean)
   is
      use Rose.Words;
      Pid : constant Process_Id := Process_Id (Mem_Pid);

      function Has (Flag : Word) return Boolean is ((Flags and Flag) /= 0);

   begin
      Success := False;

      if not Is_Valid_Process_Id (Pid)
        or else Process_Table (Pid).Num_Segments = Max_Segments
      then
         Rose.Console_IO.Put_Line ("mem: restore segment: bad request");
         Rose.Server.Fail;
         return;
      end if;

      if Overlaps (Process_Table (Pid),
                   Rose.Addresses.Virtual_Page_Address (Base),
                   Rose.Addresses.Virtual_Page_Address (Limit))
      then
         Report_Overlap (Pid,
                         Rose.Addresses.Virtual_Page_Address (Base),
                         Rose.Addresses.Virtual_Page_Address (Limit));
         Rose.Server.Fail;
         return;
      end if;

      declare
         P : Memory_Process_Record renames Process_Table (Pid);
      begin
         P.Num_Segments := P.Num_Segments + 1;
         P.Segments (P.Num_Segments) :=
           Segment_Record'
             (Base           => Rose.Addresses.Virtual_Page_Address (Base),
              Bound          => Rose.Addresses.Virtual_Page_Address (Bound),
              Limit          => Rose.Addresses.Virtual_Page_Address (Limit),
              Region         => <>,
              Owner          => Rose.Capabilities.Null_Capability,
              Region_Base    => 0,
              Region_Bound   => 0,
              Region_Offset  => 0,
              Flags          =>
                (Read       => Has (Rose.Checkpoints.Segment_Readable),
                 Write      => Has (Rose.Checkpoints.Segment_Writable),
                 Execute    => Has (Rose.Checkpoints.Segment_Executable),
                 Extensible => Has (Rose.Checkpoints.Segment_Extensible),
                 Persistent => Has (Rose.Checkpoints.Segment_Persistent),
                 Shared     => Has (Rose.Checkpoints.Segment_Shared)),
              Private_Region =>
                Has (Rose.Checkpoints.Segment_Private_Region));
      end;
      Success := True;
   end Add_Restored_Segment;

   -----------------
   -- Add_Segment --
   -----------------

   procedure Add_Segment
     (Process       : Process_Id;
      Virtual_Base  : Rose.Addresses.Virtual_Page_Address;
      Virtual_Bound : Rose.Addresses.Virtual_Page_Address;
      Region        : Rose.Capabilities.Capability;
      Region_Offset : Rose.Words.Word;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      Resizable     : Boolean;
      Success       : out Boolean;
      Private_Region : Boolean := False;
      Shared        : Boolean := False)
   is
      use type Rose.Objects.Object_Id;
      P : Memory_Process_Record renames Process_Table (Process);
      Region_Base : Rose.Objects.Page_Object_Id;
      Region_Bound : Rose.Objects.Page_Object_Id;
      New_Segment  : Segment_Record;
      Has_Range    : Boolean;

      procedure Give_Back;
      --  Mem doesn't keep a segment it refuses: delete its copy of Region,
      --  which leaves the region to the caller, who still has it.

      ---------------
      -- Give_Back --
      ---------------

      procedure Give_Back is
      begin
         Rose.System_Calls.Delete_Cap (Region);
      end Give_Back;

   begin
      Success := False;

      if P.Num_Segments = Max_Segments then
         Rose.Console_IO.Put
           ("mem: add segment: too many segments, process ");
         Rose.Console_IO.Put (Natural (Process));
         Rose.Console_IO.New_Line;
         Give_Back;
         return;
      end if;

      --  A page in two segments would be served from whichever comes
      --  first, with its region and rights, and nothing would say so
      --  (#970).

      if Overlaps (P, Virtual_Base, Virtual_Bound) then
         Report_Overlap (Process, Virtual_Base, Virtual_Bound);
         Give_Back;
         return;
      end if;

      New_Segment.Private_Region := Private_Region;
      Open_Region (New_Segment, Region);
      Get_Range (New_Segment, Region_Base, Region_Bound, Has_Range);

      --  Without its range, the segment would be read from page 0 of the
      --  store, which gives zeroes, and the process would run zeroes as
      --  code (#261). Leave the segment out, so that a fault in it is
      --  reported as an invalid page reference.

      if not Has_Range
        or else not Region_Base'Valid
        or else not Region_Bound'Valid
      then
         Rose.Console_IO.Put
           ("mem: add segment: region has no range, process ");
         Rose.Console_IO.Put (Natural (Process));
         Rose.Console_IO.New_Line;

         --  Closing the Region_Access deletes its cap, which is Region
         --  itself unless the region is private, when it was asked for.

         Rose.Interfaces.Storage.Region_Access.Client.Close
           (New_Segment.Region);
         if Private_Region then
            Give_Back;
         end if;
         return;
      end if;

      Keep_Used_Caps (New_Segment.Region, Shared);

      P.Num_Segments := P.Num_Segments + 1;
      P.Segments (P.Num_Segments) := Segment_Record'
        (Base         => Virtual_Base,
         Bound        => (if Resizable then Virtual_Base else Virtual_Bound),
         Limit        => Virtual_Bound,
         Region       => New_Segment.Region,
         Owner        => New_Segment.Owner,
         Region_Base  => Region_Base,
         Region_Bound => Region_Bound,
         Region_Offset =>
           Rose.Objects.Object_Id (Region_Offset) / Rose.Limits.Page_Size,
         Flags        =>
           (Read => Readable,
            Write => Writable,
            Execute => Executable,
            Extensible => Resizable,
            Persistent => True,
            Mem.Processes.Shared => Add_Segment.Shared),
         Private_Region => Private_Region);

      if Log_Segments then
         Report_Segment (P.Segments (P.Num_Segments));
      end if;

      Success := True;
   end Add_Segment;

   -------------------
   -- Close_Segment --
   -------------------

   procedure Close_Segment (Segment : in out Segment_Record) is
      use type Rose.Capabilities.Capability;
   begin
      --  Closing the process's own region releases it, since Region is
      --  a Resource; closing a Region_Access leaves the region (#403).
      --  Close deletes only the caps that Keep_Used_Caps kept, since
      --  deleting a null cap does nothing.

      Rose.Interfaces.Storage.Region_Access.Client.Close (Segment.Region);

      if Segment.Private_Region
        and then Segment.Owner /= Rose.Capabilities.Null_Capability
      then
         declare
            Owned : Rose.Interfaces.Storage.Region.Client.Region_Client;
         begin
            Rose.Interfaces.Storage.Region.Client.Open (Owned, Segment.Owner);
            Rose.Interfaces.Storage.Region.Client.Close (Owned);
            Segment.Owner := Rose.Capabilities.Null_Capability;
         end;
      end if;
   end Close_Segment;

   -----------------
   -- Count_Fault --
   -----------------

   procedure Count_Fault
     (Process : Process_Id;
      Kind    : Fault_Kind;
      Ticks   : Natural)
   is
      Count : Fault_Count renames Process_Table (Process).Faults (Kind);
   begin
      Count.Count := Count.Count + 1;
      Count.Ticks := Count.Ticks + Ticks;
   end Count_Fault;

   --------------
   -- End_Bank --
   --------------

   procedure End_Bank (Process : Rose.Objects.Object_Id) is
   begin
      if Have_Banks then
         Rose.Interfaces.Storage.Process_Banks.Client.End_Process
           (Process_Banks, Process);
      end if;
   end End_Bank;

   -------------------
   -- Fault_Process --
   -------------------

   procedure Fault_Process
     (Process : Process_Id;
      Message : String)
   is
      use Rose.Interfaces.Processes.Kernel_Process.Client;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Rose.Console_IO.Put_Line (Message);
      Rose.Trace.Event ("mem: fault, process", Natural (Process));
      Fault (P.Process);
      P.State := Faulted;
   end Fault_Process;

   -----------------
   -- Find_By_Oid --
   -----------------

   function Find_By_Oid
     (Oid : Rose.Objects.Object_Id)
      return Process_Id
   is
      use type Rose.Objects.Object_Id;
   begin
      if Oid /= 0 then
         for Pid in Process_Table'Range loop
            if Process_Table (Pid).State /= Available
              and then Process_Table (Pid).Oid = Oid
            then
               return Pid;
            end if;
         end loop;
      end if;
      return 0;
   end Find_By_Oid;

   ------------------
   -- Find_Process --
   ------------------

   function Find_Process
     (Identifier : Rose.Objects.Capability_Identifier)
      return Process_Id
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      for Pid in Process_Table'Range loop
         if Process_Table (Pid).State /= Available
           and then Identifier_Of (Process_Table (Pid).Oid) = Identifier
         then
            return Pid;
         end if;
      end loop;
      return 0;
   end Find_Process;

   ---------------
   -- Free_Caps --
   ---------------

   function Free_Caps return Natural is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Free_Caps_Cap = Rose.Capabilities.Null_Capability then
         return Natural'Last;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Free_Caps_Cap);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         return Natural'Last;
      end if;

      return Natural (Params.Data (0));
   end Free_Caps;

   ---------------------
   -- Get_Bank_Cap --
   ------------------

   function Get_Bank_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
   begin
      if not Is_Valid_Process_Id (Process) then
         return 0;
      end if;

      declare
         P : Memory_Process_Record renames Process_Table (Process);
      begin
         if P.Bank_Cap = 0 and then Have_Banks then
            P.Bank_Cap :=
              Interface_Cap_Of
                (Rose.Interfaces.Storage.Process_Banks.Client.Process_Bank
                   (Process_Banks, P.Oid));
         end if;
         return P.Bank_Cap;
      end;
   end Get_Bank_Cap;

   ---------------------
   -- Get_Control_Cap --
   ---------------------

   function Get_Control_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      if P.Control_Cap = 0 then
         P.Control_Cap :=
           Rose.System_Calls.Server.Create_Endpoint
             (Create_Cap   => Create_Endpoint_Cap,
              Endpoint_Id  =>
                Rose.Interfaces.Processes.Process_Control
                  .Process_Control_Interface,
              Identifier   => Identifier_Of (P.Oid));
      end if;

      return P.Control_Cap;
   end Get_Control_Cap;

   ------------------
   -- Get_Heap_Cap --
   ------------------

   function Get_Heap_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability
   is
   begin
      return Process_Table (Process).Heap;
   end Get_Heap_Cap;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id
     (Process : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id
   is
   begin
      return Process_Object_Id (Find_Process (Process));
   end Get_Object_Id;

   ------------------------
   -- Get_Page_Object_Id --
   ------------------------

   function Get_Page_Object_Id
     (Process         : Process_Id;
      Virtual_Page    : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Object_Id
   is
      use Rose.Addresses;
      use type Rose.Objects.Object_Id;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Virtual_Page >= Segment.Base
           and then Virtual_Page < Segment.Bound
           and then Segment.Region_Base /= 0
         then
            return Segment.Region_Base
              + Rose.Objects.Object_Id (Virtual_Page - Segment.Base);
         end if;
      end loop;

      return 0;
   end Get_Page_Object_Id;

   ----------------------------
   -- Get_Process_Heap_Bound --
   ----------------------------

   function Get_Process_Heap_Bound
     (Process : Process_Id)
     return Rose.Addresses.Virtual_Page_Address
   is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Segment.Flags (Extensible) then
            return Segment.Bound;
         end if;
      end loop;
      return 0;
   end Get_Process_Heap_Bound;

   --------------------
   -- Get_Process_Id --
   --------------------

   function Get_Process_Id
     (Process : Rose.Objects.Object_Id)
      return Process_Id
   is
      use type Rose.Objects.Object_Id;
   begin
      for I in Process_Table'Range loop
         if Process_Table (I).State = Active
           and then Process_Table (I).Oid = Process
         then
            return I;
         end if;
      end loop;
      return 0;
   end Get_Process_Id;

   -------------------------
   -- Get_Process_Segment --
   -------------------------

   procedure Get_Process_Segment
     (Process         : Process_Id;
      Virtual_Page    : Rose.Addresses.Virtual_Page_Address;
      Page_Object     : out Rose.Objects.Object_Id;
      Valid           : out Boolean;
      Readable        : out Boolean;
      Writable        : out Boolean;
      Executable      : out Boolean;
      Persistent      : out Boolean)
   is
      use Rose.Addresses;
      use type Rose.Objects.Object_Id;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Virtual_Page >= Segment.Base
           and then Virtual_Page < Segment.Bound
         then
            Valid := True;
            Readable := Segment.Flags (Read);
            Writable := Segment.Flags (Write);
            Executable := Segment.Flags (Execute);
            Persistent := Segment.Flags (Mem.Processes.Persistent);

            --  The pages of a transient process's writable segments are
            --  mapped as not persistent, so that they are never written
            --  to the log, by a checkpoint or to free memory (#524). They
            --  are still read from their regions (Initialize_Page).

            if P.Transient and then Segment.Flags (Write) then
               Persistent := False;
            end if;

            if Segment.Region_Base /= 0 then
               Page_Object := Segment.Region_Base
                 + Rose.Objects.Object_Id (Virtual_Page - Segment.Base);
            else
               Page_Object := 0;
            end if;
            return;
         end if;
      end loop;

      Valid := False;
   end Get_Process_Segment;

   ---------------
   -- Get_Range --
   ---------------

   procedure Get_Range
     (Segment : Segment_Record;
      Base    : out Rose.Objects.Object_Id;
      Bound   : out Rose.Objects.Object_Id;
      Success : out Boolean)
   is
   begin
      Rose.Interfaces.Storage.Region_Access.Client.Get_Range
        (Segment.Region, Base, Bound);
      Success := not Rose.Interfaces.Storage.Region_Access.Client.Has_Error;
   end Get_Range;

   -------------------------
   -- Get_Virtual_Address --
   -------------------------

   function Get_Virtual_Address
     (Process : Process_Id;
      Page    : Rose.Objects.Page_Object_Id)
      return Rose.Addresses.Virtual_Page_Address
   is
      use Rose.Objects;
      use type Rose.Addresses.Virtual_Page_Address;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Page >= Segment.Region_Base
           and then Page < Segment.Region_Bound
         then
            return Segment.Base
              + Rose.Addresses.Virtual_Page_Address
              (Page - Segment.Region_Base);
         end if;
      end loop;

      return 0;
   end Get_Virtual_Address;

   ---------------------
   -- Hand_Over_Banks --
   ---------------------

   procedure Hand_Over_Banks (Old_Oid, New_Oid : Rose.Objects.Object_Id) is
   begin
      if Have_Banks then
         Rose.Interfaces.Storage.Process_Banks.Client.Hand_Over
           (Process_Banks, Old_Oid, New_Oid);
      end if;
   end Hand_Over_Banks;

   ---------------------
   -- Initialize_Page --
   ---------------------

   procedure Initialize_Page
     (Process       : Process_Id;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      From_Log      : out Boolean)
   is
      use Rose.Addresses;
      P : Memory_Process_Record renames Process_Table (Process);
      Base : constant System.Address := Long_Buffer'Address;
      X    : constant Virtual_Address := To_Virtual_Address (Base);
      Y    : constant Virtual_Address :=
               (if X mod 4096 = 0 then X
                else X - (X mod 4096) + 4096);
      Addr : constant System.Address := To_System_Address (Y);
      Buffer : Page_Buffer;
      pragma Import (Ada, Buffer);
      for Buffer'Address use Addr;
   begin

      From_Log := False;

      for Segment of P.Segments loop
         if Virtual_Page >= Segment.Base
           and then Virtual_Page < Segment.Bound
         then
            declare
               use type Rose.Words.Word_32;
               Log_Page : constant Rose.Words.Word_32 :=
                            (if Segment.Flags (Persistent)
                             and then Segment.Flags (Write)
                             and then not Segment.Flags (Shared)
                             then Mem.Log_Index.Find (Process, Virtual_Page)
                             else 0);
               --  Only the private pages of persistent processes'
               --  writable segments are saved by checkpoints, and so
               --  indexed.
            begin
               Mem.Calls.Load_Page (Physical_Page, Buffer'Address);

               if Log_Page /= 0 then
                  Mem.Page_Log.Read (Log_Page, Buffer'Address);
                  From_Log := True;
                  Mem.Calls.Unload_Page (Physical_Page, Buffer'Address);
                  return;
               end if;
            end;

            if Segment.Flags (Persistent) then
               declare
                  use Rose.Objects;
                  Page : constant Page_Object_Id :=
                           Segment.Region_Base
                             + Object_Id (Virtual_Page - Segment.Base)
                             + Segment.Region_Offset;
               begin
                  Store_Reads := Store_Reads + 1;
                  Rose.Interfaces.Storage.Region_Access.Client.Get
                    (Item => Segment.Region,
                     Page => Page,
                     Data => Buffer);
               end;
            else
               Buffer := (others => 0);
            end if;

            Mem.Calls.Unload_Page (Physical_Page, Buffer'Address);

            return;
         end if;
      end loop;
   end Initialize_Page;

   ----------------------
   -- Interface_Cap_Of --
   ----------------------

   function Interface_Cap_Of
     (Bank : Rose.Interfaces.Storage.Bank.Client.Bank_Client)
      return Rose.Capabilities.Capability
   is
      package Client renames Rose.Interfaces.Storage.Bank.Client;

      pragma Compile_Time_Error
        (Rose.Interfaces.Storage.Bank.Subprogram_Count /= 6,
         "Bank has another operation: delete its cap below too");

      procedure Delete (Cap : Rose.Capabilities.Capability);

      ------------
      -- Delete --
      ------------

      procedure Delete (Cap : Rose.Capabilities.Capability) is
         use type Rose.Capabilities.Capability;
      begin
         if Cap /= 0 then
            Rose.System_Calls.Delete_Cap (Cap);
         end if;
      end Delete;

   begin
      Delete (Client.Get_Reserve_Storage_Cap (Bank));
      Delete (Client.Get_Create_Sub_Bank_Cap (Bank));
      Delete (Client.Get_Add_Dependent_Cap (Bank));
      Delete (Client.Get_Get_Usage_Cap (Bank));
      Delete (Client.Get_Destroy_Cap (Bank));
      Delete (Client.Get_Index_Cap (Bank));
      return Client.Get_Interface_Cap (Bank);
   end Interface_Cap_Of;

   -----------------
   -- Is_Launched --
   -----------------

   function Is_Launched (Process : Process_Id) return Boolean is
   begin
      return Process in Real_Process_Id
        and then Process > Last_Boot_Pid
        and then Process_Table (Process).State /= Available;
   end Is_Launched;

   -------------------------
   -- Is_Valid_Process_Id --
   -------------------------

   function Is_Valid_Process_Id
     (Process : Process_Id)
      return Boolean
   is
   begin
      return Process in Real_Process_Id
        and then Process_Table (Process).State = Active;
   end Is_Valid_Process_Id;

   -----------------------------
   -- Iterate_Saved_Processes --
   -----------------------------

   procedure Iterate_Saved_Processes
     (Process : not null access procedure (Pid : Process_Id))
   is
      function Is_Persistent (P : Memory_Process_Record) return Boolean;

      -------------------
      -- Is_Persistent --
      -------------------

      function Is_Persistent (P : Memory_Process_Record) return Boolean is
      begin
         for I in 1 .. P.Num_Segments loop
            if P.Segments (I).Flags (Persistent) then
               return True;
            end if;
         end loop;
         return False;
      end Is_Persistent;

   begin
      --  The boot modules are here too, but none of their segments are
      --  persistent; the processes with a persistent segment are the ones
      --  the kernel marks as persistent, except the transient ones
      --  (#524).
      for Pid in Process_Table'Range loop
         if Process_Table (Pid).State /= Available
           and then not Process_Table (Pid).Transient
           and then Is_Persistent (Process_Table (Pid))
         then
            Process (Pid);
         end if;
      end loop;
   end Iterate_Saved_Processes;

   --------------------
   -- Keep_Used_Caps --
   --------------------

   procedure Keep_Used_Caps
     (Region : in out Rose.Interfaces.Storage.Region_Access.Client
                .Region_Access_Client;
      Shared : Boolean)
   is
      package Client renames Rose.Interfaces.Storage.Region_Access.Client;

      pragma Compile_Time_Error
        (Rose.Interfaces.Storage.Region_Access.Subprogram_Count /= 4,
         "Region_Access has another operation: delete its cap below too");

      Get   : constant Rose.Capabilities.Capability :=
                Client.Get_Get_Cap (Region);
      Put   : constant Rose.Capabilities.Capability :=
                Client.Get_Put_Cap (Region);
      Empty : Client.Region_Access_Client;
   begin
      Rose.System_Calls.Delete_Cap (Client.Get_Get_Range_Cap (Region));
      Rose.System_Calls.Delete_Cap (Client.Get_Read_Cap (Region));
      Rose.System_Calls.Delete_Cap (Client.Get_Interface_Cap (Region));
      if not Shared then
         Rose.System_Calls.Delete_Cap (Put);
      end if;

      --  Open_Cap_Set leaves the interface cap as it was, and that cap is
      --  deleted: its slot can hold another cap by the time Close deletes
      --  it again. Fill an empty client, whose interface cap is null.

      Client.Open_Cap_Set
        (Client    => Empty,
         Get_Range => Rose.Capabilities.Null_Capability,
         Get       => Get,
         Put       =>
           (if Shared then Put else Rose.Capabilities.Null_Capability),
         Read      => Rose.Capabilities.Null_Capability);
      Region := Empty;
   end Keep_Used_Caps;

   --------------------
   -- Keep_Used_Caps --
   --------------------

   procedure Keep_Used_Caps (Process : in out Kernel_Process_Client) is
      package Client renames
        Rose.Interfaces.Processes.Kernel_Process.Client;

      pragma Compile_Time_Error
        (Rose.Interfaces.Processes.Kernel_Process.Subprogram_Count /= 5,
         "Kernel_Process has another operation: delete its cap below too");

      Destroy : constant Rose.Capabilities.Capability :=
                  Client.Get_Destroy_Cap (Process);
      Resume  : constant Rose.Capabilities.Capability :=
                  Client.Get_Resume_Cap (Process);
      Fault   : constant Rose.Capabilities.Capability :=
                  Client.Get_Fault_Cap (Process);
      Empty   : Kernel_Process_Client;
   begin
      Rose.System_Calls.Delete_Cap (Client.Get_Name_Cap (Process));
      Rose.System_Calls.Delete_Cap (Client.Get_Get_Object_Id_Cap (Process));
      Rose.System_Calls.Delete_Cap (Client.Get_Interface_Cap (Process));

      --  As for a Region_Access: the interface cap is deleted.

      Client.Open_Cap_Set
        (Client        => Empty,
         Name          => Rose.Capabilities.Null_Capability,
         Destroy       => Destroy,
         Get_Object_Id => Rose.Capabilities.Null_Capability,
         Resume        => Resume,
         Fault         => Fault);
      Process := Empty;
   end Keep_Used_Caps;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Process : Process_Id) is
      use Rose.Interfaces.Processes.Kernel_Process.Client;
      P : Memory_Process_Record renames Process_Table (Process);

      procedure Delete (Cap : Rose.Capabilities.Capability)
        renames Rose.System_Calls.Delete_Cap;

      procedure Report_Faults;
      --  Report the process's fault counts as trace events.

      -------------------
      -- Report_Faults --
      -------------------

      procedure Report_Faults is

         procedure Report (Text : String; Kind : Fault_Kind);

         ------------
         -- Report --
         ------------

         procedure Report (Text : String; Kind : Fault_Kind) is
         begin
            if P.Faults (Kind).Count > 0 then
               Rose.Trace.Event
                 (Text, P.Faults (Kind).Count,
                  ", ticks ", P.Faults (Kind).Ticks);
            end if;
         end Report;

      begin
         Rose.Trace.Event ("faults of process ", Natural (P.Oid));
         Report ("  region reads ", Region_Fault);
         Report ("  log reads ", Log_Fault);
         Report ("  zero-filled ", Zero_Fault);
         Report ("  cached ", Cached_Fault);
         Report ("  writes ", Write_Fault);
         Report ("  invalid ", Invalid_Fault);
      end Report_Faults;

   begin
      if P.State = Available then
         return;
      end if;

      Report_Faults;
      Destroy (P.Process);
      Close (P.Process);

      for Segment of P.Segments (1 .. P.Num_Segments) loop
         Close_Segment (Segment);
      end loop;
      P.Num_Segments := 0;

      Delete (P.Heap);
      Delete (P.Server);
      Delete (P.Interface_Cap);
      Delete (P.Control_Cap);
      P.Control_Cap := 0;
      Delete (P.Bank_Cap);
      P.Bank_Cap := 0;
      P.State := Available;
   end Kill_Process;

   ---------------
   -- Load_Page --
   ---------------

   procedure Load_Page
     (Id           : Rose.Objects.Capability_Identifier;
      Mem_Pid      : Natural;
      Virtual_Page : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      use Rose.Addresses;
      use type Rose.Objects.Object_Id;
      Pid         : constant Process_Id := Process_Id (Mem_Pid);
      V_Page      : constant Virtual_Page_Address :=
                      Virtual_Page_Address (Virtual_Page);
      Page_Object : Rose.Objects.Object_Id;
      Valid, Readable, Writable, Executable, Persistent : Boolean;
      Have_Page   : Boolean;
      Phys_Page   : Physical_Page_Address;
      From_Log    : Boolean;
      pragma Warnings (Off, From_Log);
   begin
      if not Is_Valid_Process_Id (Pid) then
         Rose.Console_IO.Put_Line ("mem: load page: bad request");
         return;
      end if;

      Get_Process_Segment
        (Pid, V_Page, Page_Object,
         Valid, Readable, Writable, Executable, Persistent);

      --  As for Restore_Page, only the pages of the process's own
      --  writable segments: a fault finds the others where it always
      --  did, by region page or store page.

      if not Valid
        or else not Writable
        or else Shared_Page (Pid, V_Page) /= 0
        or else Mem.Page_Table.Has_Element (Mem.Page_Table.Find (Pid, V_Page))
      then
         return;
      end if;

      Mem.Virtual_Map.Allocate_Page (Phys_Page, Have_Page);

      if not Have_Page then
         Rose.Console_IO.Put_Line ("mem: load page: out of memory");
         return;
      end if;

      Initialize_Page
        (Process       => Pid,
         Physical_Page => Phys_Page,
         Virtual_Page  => V_Page,
         From_Log      => From_Log);

      Mem.Virtual_Map.Map
        (Process       => Process_Table (Pid).Oid,
         Virtual_Page  => V_Page,
         Physical_Page => Phys_Page,
         Readable      => Readable,
         Writable      => Writable,
         Executable    => Executable,
         Persistent    => Persistent);
   end Load_Page;

   -----------------
   -- New_Process --
   -----------------

   function New_Process
     (Process_Cap : Rose.Capabilities.Capability;
      Bank_Cap    : Rose.Capabilities.Capability;
      Transient   : Boolean)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      Pid    : constant Process_Id :=
                 Register_Process (Process_Cap, Boot_Module => False);
      Cap    : Rose.Capabilities.Capability;
      Launch : Rose.Interfaces.Storage.Bank.Client.Bank_Client;

      procedure Give_Up;
      --  Mem has no room for the process's caps (#913). Give its slot
      --  back, and end the bank made for it. The caller ends the kernel's
      --  process, as when there is no slot.

      -------------
      -- Give_Up --
      -------------

      procedure Give_Up is
         P : Memory_Process_Record renames Process_Table (Pid);
      begin
         if P.Bank_Cap /= 0 then
            End_Bank (P.Oid);
            Rose.System_Calls.Delete_Cap (P.Bank_Cap);
         end if;
         if P.Heap /= 0 then
            Rose.System_Calls.Delete_Cap (P.Heap);
         end if;
         Rose.Interfaces.Processes.Kernel_Process.Client.Close (P.Process);
         P := No_Process;
      end Give_Up;

   begin
      if Pid = 0 then
         Rose.System_Calls.Delete_Cap (Bank_Cap);
         return 0;
      end if;

      Process_Table (Pid).Transient := Transient;

      --  The process's own bank, a sub-bank of the one it is launched
      --  against (ADR 0015).  The store records that it is the process's,
      --  so mem need not keep the cap across a restore.

      if Have_Banks and then Bank_Cap /= 0 then
         Rose.Interfaces.Storage.Bank.Client.Open (Launch, Bank_Cap);
         Process_Table (Pid).Bank_Cap :=
           Interface_Cap_Of
             (Rose.Interfaces.Storage.Process_Banks.Client.Create_Process_Bank
                (Process_Banks,
                 Parent  => Rose.Interfaces.Storage.Bank.Client.Index (Launch),
                 Process   => Process_Table (Pid).Oid,
                 Transient => Transient));
         Rose.Interfaces.Storage.Bank.Client.Close (Launch);
      else
         Rose.System_Calls.Delete_Cap (Bank_Cap);
      end if;

      Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Cap   => Create_Endpoint_Cap,
           Endpoint_Id  =>
             Rose.Interfaces.Processes.Process.Process_Interface,
           Identifier   => Process_Identifier (Pid));

      if Cap = 0 then
         Give_Up;
         return 0;
      end if;

      Process_Table (Pid).Interface_Cap := Cap;
      return Cap;
   end New_Process;

   -----------------------
   -- New_Restored_Page --
   -----------------------

   procedure New_Restored_Page
     (Page      : System.Storage_Elements.Storage_Array;
      Physical  : out Rose.Addresses.Physical_Page_Address;
      Success   : out Boolean)
   is
      use Rose.Addresses;
   begin
      Mem.Virtual_Map.Allocate_Page (Physical, Success);

      if not Success then
         return;
      end if;

      declare
         Base : constant System.Address := Long_Buffer'Address;
         X    : constant Virtual_Address := To_Virtual_Address (Base);
         Y    : constant Virtual_Address :=
                  (if X mod 4096 = 0 then X
                   else X - (X mod 4096) + 4096);
         Buffer : Page_Buffer;
         pragma Import (Ada, Buffer);
         for Buffer'Address use To_System_Address (Y);
      begin
         Mem.Calls.Load_Page (Physical, Buffer'Address);
         Buffer := Page;
         Mem.Calls.Unload_Page (Physical, Buffer'Address);
      end;
   end New_Restored_Page;

   -----------------
   -- Open_Region --
   -----------------

   procedure Open_Region
     (Segment : in out Segment_Record;
      Cap     : Rose.Capabilities.Capability)
   is
      Access_Cap : Rose.Capabilities.Capability := Cap;
   begin
      if Segment.Private_Region then

         --  Ask the Region for its Region_Access, which is its first
         --  ancestor, as Rose.Interfaces.Storage.Region.Client.Conversions
         --  .To_Region_Access does, without opening a client of the Region
         --  itself.

         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Cap);
            Rose.System_Calls.Send_Word (Params, Natural (1));
            Rose.System_Calls.Receive_Caps (Params, 1);
            Rose.System_Calls.Invoke_Capability (Params);
            Access_Cap :=
              (if Params.Control.Flags (Rose.Invocation.Error)
               then Rose.Capabilities.Null_Capability
               else Params.Caps (0));
         end;
         Segment.Owner := Cap;
      end if;

      Rose.Interfaces.Storage.Region_Access.Client.Open
        (Segment.Region, Access_Cap);
   end Open_Region;

   --------------
   -- Overlaps --
   --------------

   function Overlaps
     (P     : Memory_Process_Record;
      Base  : Rose.Addresses.Virtual_Page_Address;
      Bound : Rose.Addresses.Virtual_Page_Address)
      return Boolean
   is
      use type Rose.Addresses.Virtual_Page_Address;
   begin
      for Segment of P.Segments (1 .. P.Num_Segments) loop
         if Base < Segment.Limit and then Segment.Base < Bound then
            return True;
         end if;
      end loop;
      return False;
   end Overlaps;

   -----------------
   -- Parent_Bank --
   -----------------

   function Parent_Bank
     (Process : Process_Id)
      return Rose.Capabilities.Capability
   is
   begin
      if not Have_Banks or else Process not in Real_Process_Id then
         return 0;
      end if;
      return Interface_Cap_Of
        (Rose.Interfaces.Storage.Process_Banks.Client.Parent_Bank
           (Process_Banks, Process_Table (Process).Oid));
   end Parent_Bank;

   ------------------------
   -- Process_Identifier --
   ------------------------

   function Process_Identifier
     (Process : Process_Id)
      return Rose.Objects.Capability_Identifier
   is (Identifier_Of (Process_Table (Process).Oid));

   -----------------------
   -- Process_Object_Id --
   -----------------------

   function Process_Object_Id
     (Process : Process_Id)
      return Rose.Objects.Object_Id
   is
   begin
      if Process not in Real_Process_Id
        or else Process_Table (Process).State = Available
      then
         return Rose.Objects.Null_Object_Id;
      else
         return Process_Table (Process).Oid;
      end if;
   end Process_Object_Id;

   -----------------------------
   -- Published_Interface_Cap --
   -----------------------------

   function Published_Interface_Cap
     (Process : Process_Id)
      return Rose.Capabilities.Capability
   is
   begin
      return Process_Table (Process).Server;
   end Published_Interface_Cap;

   -----------------
   -- Region_Page --
   -----------------

   function Region_Page
     (Process      : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Object_Id
   is
      use Rose.Addresses;
      use Rose.Objects;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments loop
         if Virtual_Page >= Segment.Base
           and then Virtual_Page < Segment.Bound
           and then Segment.Region_Base /= 0
         then
            return Segment.Region_Base
              + Object_Id (Virtual_Page - Segment.Base)
              + Segment.Region_Offset;
         end if;
      end loop;
      return 0;
   end Region_Page;

   ----------------------
   -- Register_Process --
   ----------------------

   function Register_Process
     (Process_Cap : Rose.Capabilities.Capability;
      Boot_Module : Boolean)
      return Process_Id
   is
      First  : constant Real_Process_Id :=
                 (if Boot_Module then Real_Process_Id'First
                  else Last_Boot_Pid + 1);
      Last   : constant Real_Process_Id :=
                 (if Boot_Module then Last_Boot_Pid
                  else Real_Process_Id'Last);
      Next_Pid : Real_Process_Id :=
                   (if Boot_Module then Next_Boot_Pid
                    else Next_Launched_Pid);
      use type Rose.Capabilities.Capability;
      Start  : constant Process_Id := Next_Pid;
      Client : Rose.Interfaces.Processes.Kernel_Process.Client
        .Kernel_Process_Client;
   begin
      --  Few caps left: refuse the launch while every caller can still
      --  have a reply cap. Like running out of slots below, the caller
      --  fails the launch, and nothing is written (#1131, #915).

      if not Boot_Module and then Free_Caps < Launch_Reserve then
         Rose.System_Calls.Delete_Cap (Process_Cap);
         return 0;
      end if;

      while Process_Table (Next_Pid).State /= Available loop
         if Next_Pid = Last then
            Next_Pid := First;
         else
            Next_Pid := Next_Pid + 1;
         end if;
         if Next_Pid = Start then
            --  Out of processes. The caller fails the launch.  Mem never
            --  writes to the console while it serves a launch: the
            --  console may be waiting on mem, for a page or for a
            --  checkpoint, and the system would stop (#915).
            Rose.System_Calls.Delete_Cap (Process_Cap);
            return 0;
         end if;
      end loop;

      if Boot_Module then
         Next_Boot_Pid := Next_Pid;
      else
         Next_Launched_Pid := Next_Pid;
      end if;

      Rose.Interfaces.Processes.Kernel_Process.Client
        .Open (Client, Process_Cap);

      declare
         Oid : constant Rose.Objects.Object_Id :=
                 Rose.Interfaces.Processes.Kernel_Process.Client
                   .Get_Object_Id (Client);
      begin
         --  Mem may have no room for the client's caps, and then the
         --  call fails (#913).

         if Rose.Interfaces.Processes.Kernel_Process.Client.Has_Error
           or else Find_Process (Identifier_Of (Oid)) /= 0
         then
            Rose.Interfaces.Processes.Kernel_Process.Client.Close (Client);
            return 0;
         end if;
      end;

      declare
         Oid : constant Rose.Objects.Object_Id :=
                 Rose.Interfaces.Processes.Kernel_Process.Client
                   .Get_Object_Id (Client);
         Heap_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Endpoint
                        (Create_Cap   => Create_Endpoint_Cap,
                         Endpoint_Id  =>
                           Rose.Interfaces.Processes.Heap.Heap_Interface,
                         Identifier   => Identifier_Of (Oid));
      begin
         if Heap_Cap = 0 then
            Rose.Interfaces.Processes.Kernel_Process.Client.Close (Client);
            return 0;
         end if;

         Keep_Used_Caps (Client);
         Process_Table (Next_Pid) := No_Process;
         Process_Table (Next_Pid).State := Active;
         Process_Table (Next_Pid).Oid := Oid;
         Process_Table (Next_Pid).Heap := Heap_Cap;
         Process_Table (Next_Pid).Process := Client;
      end;

      Mem.Log_Index.Forget (Next_Pid);

      return Next_Pid;
   end Register_Process;

   -----------------------------
   -- Release_Shared_Segments --
   -----------------------------

   procedure Release_Shared_Segments (Process : Process_Id) is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments (1 .. P.Num_Segments) loop
         if Segment.Flags (Shared) then
            Unmap_Segment_Pages (Process, Segment, Unmap => False);
         end if;
      end loop;
   end Release_Shared_Segments;

   --------------------
   -- Remove_Segment --
   --------------------

   procedure Remove_Segment
     (Process      : Process_Id;
      Virtual_Base : Rose.Addresses.Virtual_Page_Address)
   is
      use type Rose.Addresses.Virtual_Page_Address;
      P     : Memory_Process_Record renames Process_Table (Process);
      Empty : Segment_Record;
   begin
      for I in 1 .. P.Num_Segments loop
         if P.Segments (I).Base = Virtual_Base then
            Unmap_Segment_Pages (Process, P.Segments (I), Unmap => True);
            Close_Segment (P.Segments (I));
            P.Segments (I .. P.Num_Segments - 1) :=
              P.Segments (I + 1 .. P.Num_Segments);
            P.Segments (P.Num_Segments) := Empty;
            P.Num_Segments := P.Num_Segments - 1;
            return;
         end if;
      end loop;

      Rose.Console_IO.Put ("mem: remove segment: no segment there, process ");
      Rose.Console_IO.Put (Natural (Process));
      Rose.Console_IO.New_Line;
   end Remove_Segment;

   --------------------
   -- Report_Overlap --
   --------------------

   procedure Report_Overlap
     (Process : Process_Id;
      Base    : Rose.Addresses.Virtual_Page_Address;
      Bound   : Rose.Addresses.Virtual_Page_Address)
   is
      use type Rose.Addresses.Virtual_Page_Address;
      use type Rose.Words.Word;
      P : Memory_Process_Record renames Process_Table (Process);

      procedure Put_Range
        (From, To : Rose.Addresses.Virtual_Page_Address);

      ---------------
      -- Put_Range --
      ---------------

      procedure Put_Range
        (From, To : Rose.Addresses.Virtual_Page_Address)
      is
      begin
         Rose.Console_IO.Put (Rose.Words.Word (From) * 4096);
         Rose.Console_IO.Put ("-");
         Rose.Console_IO.Put (Rose.Words.Word (To) * 4096);
      end Put_Range;

   begin
      Rose.Console_IO.Put ("mem: add segment: process ");
      Rose.Console_IO.Put (Natural (Process));
      Rose.Console_IO.Put (": ");
      Put_Range (Base, Bound);
      Rose.Console_IO.Put (" overlaps ");
      for Segment of P.Segments (1 .. P.Num_Segments) loop
         if Base < Segment.Limit and then Segment.Base < Bound then
            Put_Range (Segment.Base, Segment.Limit);
            exit;
         end if;
      end loop;
      Rose.Console_IO.New_Line;
   end Report_Overlap;

   --------------------
   -- Report_Segment --
   --------------------

   procedure Report_Segment (Segment : Segment_Record) is

      use type Rose.Words.Word;
      use type Rose.Objects.Object_Id;

      procedure Put (Flag : Segment_Flag;
                     Char : Character);

      ---------
      -- Put --
      ---------

      procedure Put (Flag : Segment_Flag;
                     Char : Character)
      is
      begin
         if Segment.Flags (Flag) then
            Rose.Console_IO.Put (Char);
         else
            Rose.Console_IO.Put ('-');
         end if;
      end Put;

      Base : constant Rose.Words.Word :=
               Rose.Words.Word (Segment.Region_Base + Segment.Region_Offset)
               * 4096;
      Bound : constant Rose.Words.Word :=
                Base + (Rose.Words.Word (Segment.Bound)
                        - Rose.Words.Word (Segment.Base)) * 4096;

   begin
      Rose.Console_IO.Put ("mem: region ");
      Rose.Console_IO.Put (Base);
      Rose.Console_IO.Put (" - ");
      Rose.Console_IO.Put (Bound);

      Rose.Console_IO.Put ("; flags ");
      Put (Read, 'r');
      Put (Write, 'w');
      Put (Execute, 'x');
      Put (Extensible, 's');
      Put (Persistent, 'p');
      Put (Shared, 'S');
      Rose.Console_IO.New_Line;
   end Report_Segment;

   --------------------
   -- Resize_Segment --
   --------------------

   procedure Resize_Segment
     (Process           : Process_Id;
      New_Virtual_Bound : Rose.Addresses.Virtual_Page_Address)
   is
   begin

      if Is_Valid_Process_Id (Process) then
         declare
            use Rose.Addresses;
            P : Memory_Process_Record renames Process_Table (Process);
         begin
            for Segment of P.Segments loop
               if Segment.Flags (Extensible)
                 and then New_Virtual_Bound in Segment.Base .. Segment.Limit
               then
                  if New_Virtual_Bound < Segment.Bound then
                     for I in New_Virtual_Bound .. Segment.Bound - 1 loop
                        Mem.Calls.Unmap (P.Oid, I);
                     end loop;
                  end if;

                  Segment.Bound := New_Virtual_Bound;
                  return;
               end if;
            end loop;
         end;
      end if;
   end Resize_Segment;

   ------------------
   -- Restore_Page --
   ------------------

   procedure Restore_Page
     (Id           : Rose.Objects.Capability_Identifier;
      Mem_Pid      : Natural;
      Virtual_Page : Rose.Words.Word;
      Page         : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      use Rose.Addresses;
      use type Rose.Objects.Object_Id;
      Pid       : constant Process_Id := Process_Id (Mem_Pid);
      V_Page    : constant Virtual_Page_Address :=
                    Virtual_Page_Address (Virtual_Page);
      Page_Object : Rose.Objects.Object_Id;
      Valid, Readable, Writable, Executable, Persistent : Boolean;
      Have_Page : Boolean;
      Phys_Page : Physical_Page_Address;
   begin
      if not Is_Valid_Process_Id (Pid) or else Page'Length /= 4096 then
         Rose.Console_IO.Put_Line ("mem: restore page: bad request");
         return;
      end if;

      Get_Process_Segment
        (Pid, V_Page, Page_Object,
         Valid, Readable, Writable, Executable, Persistent);

      if not Valid or else not Writable then
         Rose.Console_IO.Put ("mem: restore page: pid ");
         Rose.Console_IO.Put (Natural (Pid));
         Rose.Console_IO.Put (" page ");
         Rose.Console_IO.Put (Rose.Words.Word_32 (Virtual_Page));
         Rose.Console_IO.Put_Line (" is not in a writable segment");
         return;
      end if;

      --  A shared segment's pages are restored by store page
      --  (Restore_Shared_Page). Checkpoints from before ADR 0025's
      --  section 4 saved them by process too; those copies are ignored.

      if Shared_Page (Pid, V_Page) /= 0 then
         return;
      end if;

      if Mem.Page_Table.Has_Element (Mem.Page_Table.Find (Pid, V_Page)) then
         Rose.Console_IO.Put_Line ("mem: restore page: already restored");
         return;
      end if;

      New_Restored_Page (Page, Phys_Page, Have_Page);
      if not Have_Page then
         Rose.Console_IO.Put_Line ("mem: restore page: out of memory");
         return;
      end if;

      Mem.Virtual_Map.Map
        (Process       => Process_Table (Pid).Oid,
         Virtual_Page  => V_Page,
         Physical_Page => Phys_Page,
         Readable      => Readable,
         Writable      => Writable,
         Executable    => Executable,
         Persistent    => Persistent);

      --  The page is in the log only in older entries, so it is dirty:
      --  the next checkpoint saves it with the rest. Past the limit of
      --  changed pages, some are paged out first; a restore can't wait
      --  for a checkpoint, so if none can be, it goes past it (#998).

      declare
         Position : constant Mem.Page_Table.Cursor :=
                      Mem.Page_Table.Find (Pid, V_Page);
         Room     : Boolean;
         pragma Warnings (Off, Room);
      begin
         if Mem.Page_Table.Would_Pass_Dirty_Limit (Position) then
            Mem.Virtual_Map.Make_Room_For_Change (Room);
         end if;
         Mem.Page_Table.Set_Writable (Position);
      end;
   end Restore_Page;

   ---------------------
   -- Restore_Process --
   ---------------------

   procedure Restore_Process
     (Id              : Rose.Objects.Capability_Identifier;
      Process         : Rose.Capabilities.Capability;
      Mem_Pid         : Natural;
      Index_Directory : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Pid    : constant Process_Id := Process_Id (Mem_Pid);
      Client : Rose.Interfaces.Processes.Kernel_Process.Client
        .Kernel_Process_Client;
   begin
      if Pid not in Real_Process_Id
        or else Process_Table (Pid).State /= Available
      then
         Rose.Console_IO.Put_Line ("mem: restore process: slot in use");
         return;
      end if;

      Rose.Interfaces.Processes.Kernel_Process.Client.Open (Client, Process);

      --  The heap and process caps the process holds carry its identifier,
      --  which comes from its object id, so they are made again in the
      --  same way as when it was created.

      declare
         P   : Memory_Process_Record renames Process_Table (Pid);
         Oid : constant Rose.Objects.Object_Id :=
                 Rose.Interfaces.Processes.Kernel_Process.Client
                   .Get_Object_Id (Client);
      begin
         P := No_Process;
         P.State := Active;
         P.Oid := Oid;
         P.Heap :=
           Rose.System_Calls.Server.Create_Endpoint
             (Create_Cap   => Create_Endpoint_Cap,
              Endpoint_Id  => Rose.Interfaces.Processes.Heap.Heap_Interface,
              Identifier   => Identifier_Of (Oid));
         Keep_Used_Caps (Client);
         P.Process := Client;
         P.Interface_Cap :=
           Rose.System_Calls.Server.Create_Endpoint
             (Create_Cap   => Create_Endpoint_Cap,
              Endpoint_Id  =>
                Rose.Interfaces.Processes.Process.Process_Interface,
              Identifier   => Identifier_Of (Oid));
      end;

      --  Its pages are read from the log through its index when it faults
      --  on them (ADR 0028, section 9). A checkpoint without an index
      --  gives them all with Restore_Page instead, as dirty pages, so the
      --  next checkpoint writes a complete index.

      Mem.Log_Index.Set_Directory
        (Pid, Rose.Words.Word_32 (Index_Directory));
   end Restore_Process;

   ---------------------
   -- Restore_Segment --
   ---------------------

   procedure Restore_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Mem_Pid       : Natural;
      Base          : Rose.Words.Word;
      Bound         : Rose.Words.Word;
      Limit         : Rose.Words.Word;
      Flags         : Rose.Words.Word;
      Region        : Rose.Capabilities.Capability;
      Region_Offset : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Pid       : constant Process_Id := Process_Id (Mem_Pid);
      Has_Range : Boolean;
      Added     : Boolean;
   begin
      Add_Restored_Segment (Mem_Pid, Base, Bound, Limit, Flags, Added);

      if not Added then
         Rose.System_Calls.Delete_Cap (Region);
         return;
      end if;

      if Is_Valid_Process_Id (Pid)
        and then Process_Table (Pid).Num_Segments > 0
      then
         declare
            P            : Memory_Process_Record renames Process_Table (Pid);
            Segment      : Segment_Record renames P.Segments (P.Num_Segments);
            Region_Base  : Rose.Objects.Object_Id;
            Region_Bound : Rose.Objects.Object_Id;
         begin
            Open_Region (Segment, Region);
            Get_Range (Segment, Region_Base, Region_Bound, Has_Range);
            Keep_Used_Caps (Segment.Region, Segment.Flags (Shared));
            if Has_Range then
               Segment.Region_Base := Region_Base;
               Segment.Region_Bound := Region_Bound;
            end if;
            Segment.Region_Offset := Rose.Objects.Object_Id (Region_Offset);
         end;
      end if;
   end Restore_Segment;

   -------------------------
   -- Restore_Shared_Page --
   -------------------------

   procedure Restore_Shared_Page
     (Id         : Rose.Objects.Capability_Identifier;
      Store_Page : Rose.Objects.Object_Id;
      Page       : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      Physical : Rose.Addresses.Physical_Page_Address;
      Success  : Boolean;
   begin
      if Page'Length /= 4096 then
         Rose.Console_IO.Put_Line ("mem: restore shared page: bad request");
         return;
      end if;

      New_Restored_Page (Page, Physical, Success);
      if not Success then
         Rose.Console_IO.Put_Line ("mem: restore shared page: out of memory");
         return;
      end if;

      Mem.Shared_Pages.Restore (Store_Page, Physical, Success);
      if not Success then
         Rose.Console_IO.Put_Line
           ("mem: restore shared page: already restored, or table full");
         Mem.Physical_Map.Deallocate_Page (Physical);
      end if;
   end Restore_Shared_Page;

   --------------------------
   -- Restore_Zero_Segment --
   --------------------------

   procedure Restore_Zero_Segment
     (Id      : Rose.Objects.Capability_Identifier;
      Mem_Pid : Natural;
      Base    : Rose.Words.Word;
      Bound   : Rose.Words.Word;
      Limit   : Rose.Words.Word;
      Flags   : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Added : Boolean;
   begin
      Add_Restored_Segment (Mem_Pid, Base, Bound, Limit, Flags, Added);
   end Restore_Zero_Segment;

   --------------------
   -- Resume_Process --
   --------------------

   procedure Resume_Process
     (Process         : Process_Id)
   is
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Rose.Interfaces.Processes.Kernel_Process.Client.Resume (P.Process);
   end Resume_Process;

   ------------------
   -- Save_Process --
   ------------------

   procedure Save_Process
     (Process : Process_Id;
      Item    : in out Rose.Checkpoints.Mem_Process_Record)
   is
      use Rose.Words;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      Item.Magic := Rose.Checkpoints.Mem_Process_Magic;
      Item.Oid := Word_64 (P.Oid);
      Item.Mem_Pid := Word_32 (Process);
      Item.State := Process_State'Pos (P.State);
      Item.Segment_Count := Word_32 (P.Num_Segments);
      Item.Index_Directory := Mem.Log_Index.Saved_Directory (Process);

      for I in 1 .. P.Num_Segments loop
         declare
            Segment : Segment_Record renames P.Segments (I);
            Saved   : Rose.Checkpoints.Saved_Segment renames
                        Item.Segments (Positive (I));
         begin
            Saved.Base := Word_32 (Segment.Base);
            Saved.Bound := Word_32 (Segment.Bound);
            Saved.Limit := Word_32 (Segment.Limit);
            Saved.Region_Base := Word_64 (Segment.Region_Base);
            Saved.Region_Bound := Word_64 (Segment.Region_Bound);
            Saved.Region_Offset := Word_64 (Segment.Region_Offset);
            Saved.Flags :=
              (if Segment.Flags (Read)
               then Rose.Checkpoints.Segment_Readable else 0)
              or (if Segment.Flags (Write)
                  then Rose.Checkpoints.Segment_Writable else 0)
              or (if Segment.Flags (Execute)
                  then Rose.Checkpoints.Segment_Executable else 0)
              or (if Segment.Flags (Extensible)
                  then Rose.Checkpoints.Segment_Extensible else 0)
              or (if Segment.Flags (Persistent)
                  then Rose.Checkpoints.Segment_Persistent else 0)
              or (if Segment.Private_Region
                  then Rose.Checkpoints.Segment_Private_Region else 0)
              or (if Segment.Flags (Shared)
                  then Rose.Checkpoints.Segment_Shared else 0);
         end;
      end loop;
   end Save_Process;

   ---------------
   -- Set_Ended --
   ---------------

   procedure Set_Ended (Ended_Cap : Rose.Capabilities.Capability) is
   begin
      if Have_Banks then
         Rose.Interfaces.Storage.Process_Banks.Client.Set_Ended
           (Process_Banks, Ended_Cap);
      end if;
   end Set_Ended;

   -----------------------
   -- Set_Process_Banks --
   -----------------------

   procedure Set_Process_Banks (Banks_Cap : Rose.Capabilities.Capability) is
   begin
      Rose.Interfaces.Storage.Process_Banks.Client
        .Open (Process_Banks, Banks_Cap);
      Have_Banks := True;
   end Set_Process_Banks;

   ---------------------------------
   -- Set_Published_Interface_Cap --
   ---------------------------------

   procedure Set_Published_Interface_Cap
     (Process : Process_Id;
      Cap     : Rose.Capabilities.Capability)
   is
   begin
      Process_Table (Process).Server := Cap;
   end Set_Published_Interface_Cap;

   -----------------
   -- Shared_Page --
   -----------------

   function Shared_Page
     (Process      : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Object_Id
   is
      use Rose.Addresses;
      use Rose.Objects;
      P : Memory_Process_Record renames Process_Table (Process);
   begin
      for Segment of P.Segments (1 .. P.Num_Segments) loop
         if Virtual_Page >= Segment.Base
           and then Virtual_Page < Segment.Bound
         then
            if Segment.Flags (Shared) and then Segment.Region_Base /= 0 then
               return Segment.Region_Base
                 + Object_Id (Virtual_Page - Segment.Base)
                 + Segment.Region_Offset;
            end if;
            return 0;
         end if;
      end loop;
      return 0;
   end Shared_Page;

   ----------------------
   -- Store_Read_Count --
   ----------------------

   function Store_Read_Count return Natural is
   begin
      return Store_Reads;
   end Store_Read_Count;

   ------------------
   -- Swap_Records --
   ------------------

   procedure Swap_Records (Old, New_Version : Process_Id) is
      A : Memory_Process_Record renames Process_Table (Old);
      B : Memory_Process_Record renames Process_Table (New_Version);
      Oid           : constant Rose.Objects.Object_Id := A.Oid;
      Process       : constant Kernel_Process_Client := A.Process;
      Heap          : constant Rose.Capabilities.Capability := A.Heap;
      Server        : constant Rose.Capabilities.Capability := A.Server;
      Interface_Cap : constant Rose.Capabilities.Capability :=
                        A.Interface_Cap;
      Control_Cap   : constant Rose.Capabilities.Capability :=
                        A.Control_Cap;
   begin
      A.Oid := B.Oid;
      A.Process := B.Process;
      A.Heap := B.Heap;
      A.Server := B.Server;
      A.Interface_Cap := B.Interface_Cap;
      A.Control_Cap := B.Control_Cap;

      B.Oid := Oid;
      B.Process := Process;
      B.Heap := Heap;
      B.Server := Server;
      B.Interface_Cap := Interface_Cap;
      B.Control_Cap := Control_Cap;
   end Swap_Records;

   ----------------
   -- Take_Ended --
   ----------------

   function Take_Ended return Rose.Objects.Object_Id is
   begin
      if not Have_Banks then
         return 0;
      end if;
      return Rose.Interfaces.Storage.Process_Banks.Client
        .Take_Ended (Process_Banks);
   end Take_Ended;

   -------------------------
   -- Unmap_Segment_Pages --
   -------------------------

   procedure Unmap_Segment_Pages
     (Process : Process_Id;
      Segment : Segment_Record;
      Unmap   : Boolean)
   is
      use Rose.Addresses;

      procedure Free (Position : Mem.Page_Table.Cursor);

      procedure Write_Back
        (Virtual_Page  : Virtual_Page_Address;
         Physical_Page : Physical_Page_Address);
      --  Put the contents of Physical_Page, which Virtual_Page of the
      --  segment maps, into the segment's region.

      ----------
      -- Free --
      ----------

      procedure Free (Position : Mem.Page_Table.Cursor) is
         Virtual  : constant Virtual_Page_Address :=
                      Mem.Page_Table.Virtual_Address (Position);
         Physical : constant Physical_Page_Address :=
                      Mem.Page_Table.Physical_Address (Position);
         Cached, Last, Modified : Boolean;
      begin
         if Unmap then
            Mem.Calls.Unmap (Process_Object_Id (Process), Virtual);
         end if;
         Mem.Page_Table.Delete (Position);

         if Mem.Shared_Pages.Is_Shared (Physical) then
            Mem.Shared_Pages.Release (Physical, Last, Modified);
            if Last then
               if Modified then
                  Write_Back (Virtual, Physical);
               end if;
               Mem.Physical_Map.Deallocate_Page (Physical);
            end if;
         else
            Mem.Page_Cache.Release (Physical, Cached);
            if not Cached then
               Mem.Physical_Map.Deallocate_Page (Physical);
            end if;
         end if;
      end Free;

      ----------------
      -- Write_Back --
      ----------------

      procedure Write_Back
        (Virtual_Page  : Virtual_Page_Address;
         Physical_Page : Physical_Page_Address)
      is
         use Rose.Objects;
         Page : constant Page_Object_Id :=
                  Segment.Region_Base
                  + Object_Id (Virtual_Page - Segment.Base)
                  + Segment.Region_Offset;

         procedure Put (Address : System.Address);

         ---------
         -- Put --
         ---------

         procedure Put (Address : System.Address) is
            Data : Page_Buffer with Import, Address => Address;
         begin
            Rose.Interfaces.Storage.Region_Access.Client.Put
              (Segment.Region, Page, Data);

            --  The store has no space for the page (#945). The region
            --  keeps what it last had there, so the segment goes on.

            if Rose.Interfaces.Storage.Region_Access.Client.Has_Error then
               Rose.Console_IO.Put ("mem: shared page not written back: ");
               Rose.Console_IO.Put (Page);
               Rose.Console_IO.New_Line;
            end if;
         end Put;

      begin
         Mem.Virtual_Map.With_Page (Physical_Page, Put'Access);
      end Write_Back;

   begin
      Mem.Page_Table.Iterate_Process_Pages
        (Process, Segment.Base, Segment.Limit, Free'Access);
   end Unmap_Segment_Pages;

end Mem.Processes;
