with System.Storage_Elements;

with Rose.Boot.Console;
with Rose.Interfaces.Memory;
with Rose.Interfaces.Process;
with Rose.Interfaces.Restorable;
with Rose.Interfaces.Signalled;

with Rose.Arch.Interrupts;
with Rose.Kernel.Cap_Tables;
with Rose.Kernel.Derivations;
with Rose.Kernel.Page_Table;
with Rose.Kernel.Processes.Debug;
with Rose.Kernel.Processes.Queue;
with Rose.Kernel.Processes;

with Rose.Kernel.Heap;

with Rose.Kernel.Panic;
with Rose.Invocation.Trace;

with Rose.Arch;

package body Rose.Kernel.Processes is

   FPU_Buffer : Rose.Kernel.Arch.FPU_State
     with Alignment => 16;
   --  FXSAVE and FXRSTOR need an area aligned to 16, which a process
   --  table entry is not, so the state passes through here.

   Log_Shared_Buffers : constant Boolean := False;
   Log_State_Changes  : constant Boolean := False;

   Active_Checkpoint : Boolean := False;

   type Checked_Record is
      record
         Address : Rose.Words.Word_32 := 0;
         Changes : Rose.Kernel.Page_Table.Change_Count := 0;
         Valid   : Boolean := False;
      end record;

   Checked_Records : array (Kernel_Process_Table'Range) of Checked_Record;
   --  For each process, the last invocation record that passed the check,
   --  and the page table change count then (#717). It is kept out of the
   --  process table, so that a checkpoint never saves it.

   type Handover_Record is
      record
         Held    : Boolean := False;
         Except  : Rose.Objects.Endpoint_Index := 0;
         Retired : Boolean := False;
      end record;

   Handovers : array (Kernel_Process_Table'Range) of Handover_Record;
   --  Each process's part in a handover (Hold_Calls, Swap_Processes). It
   --  is kept out of the process table, as Checked_Records is: a
   --  checkpoint never starts while a handover is in progress.

   procedure Release_Entry (Pid : Process_Id);

   procedure Add_Reference
     (Layout : Rose.Capabilities.Layout.Capability_Layout);
   --  A slot now holds Layout.

   procedure Drop_Reference
     (Layout : Rose.Capabilities.Layout.Capability_Layout);
   --  A slot no longer holds Layout.

   function Is_Valid_Derivation
     (Layout : Rose.Capabilities.Layout.Capability_Layout)
      return Boolean;
   --  The record that Layout was derived from, if any, and the records
   --  above it have not been revoked.

   function Is_Repeated_Single_Use
     (Pid       : Process_Id;
      Params    : Rose.Invocation.Invocation_Record;
      Cap_Index : Rose.Invocation.Capability_Index)
      return Boolean;
   --  The cap that Params sends at Cap_Index is a single-use cap of Pid's
   --  that it sends at an earlier index too.  It arrives once, at the
   --  first (ADR 0032, #864).

   procedure Move_Single_Use_Caps
     (Pid    : Process_Id;
      Params : Rose.Invocation.Invocation_Record);
   --  Params, from Pid, has been delivered with its caps.  A single-use
   --  cap, such as a reply cap, is moved, not copied, so that it is still
   --  used once: empty each one in Pid's table.  The slot stays held, so
   --  that its number isn't given to another cap that Pid would take for
   --  it; nothing can be sent or invoked through it, and deleting it
   --  frees it (ADR 0032, #864).

   procedure Wake_Waiting_Processes (Pid : Process_Id);
   --  Wake each process that is waiting on Pid, which is being killed,
   --  as if its call had failed: those blocked sending to it, and those
   --  waiting for its reply, which it holds reply caps for.

   procedure Wake_With_Error
     (Pid   : Process_Id;
      Error : Rose.Invocation.Invocation_Error);
   --  Make the blocked process Pid ready, with its call failing with
   --  Error.
   --  Take the process in entry Pid off the run queue and every wait
   --  list, give its kernel pages back to the heap, and clear the entry
   --  as it was before its first use, keeping its pid and object id.

   procedure Show_Address
     (Addr : Rose.Words.Word);
   pragma Export (C, Show_Address, "debug_show_address");

   Current_Page_Fault_Count  : Natural := 0;
   Page_Fault_Params         : aliased Rose.Invocation.Invocation_Record;

   Heap_Request_Pending : Boolean := False;
   Heap_Request_Amount  : Rose.Addresses.Physical_Bytes := 0;
   Last_Fault_Pid       : Process_Id := Null_Process_Id;
   --  Page faults and requests for heap memory go to mem only while it
   --  waits to receive. If it waits for the reply to a call, they would
   --  be taken as that reply (#270), so they wait for its next receive.

   function Can_Send_To_Memory return Boolean;
   --  True if mem waits to receive, and not for the reply to a call.

   procedure Send_Heap_Request;
   --  Ask mem for Heap_Request_Amount of memory.

   procedure Send_Page_Fault (Pid : Process_Id);
   --  Tell mem about the page fault recorded for Pid.

   procedure Send_Exit (Pid : Process_Id);
   --  Tell mem that Pid has exited, as if Pid had called Exit_Process on
   --  its Process interface.

   Page_Fault_Address : Rose.Words.Word_32;
   pragma Import (C, Page_Fault_Address, "page_fault_address");

   function Handle_Page_Fault
     (Code : Rose.Words.Word)
      return Rose.Kernel.Interrupts.Interrupt_Handler_Status;

   procedure Share_Page
     (From_Process : Rose.Kernel.Processes.Process_Id;
      To_Process   : Rose.Kernel.Processes.Process_Id;
      Address      : Rose.Addresses.Virtual_Page_Address;
      Writable     : Boolean;
      Shared       : out Rose.Addresses.Virtual_Page_Address);
   --  Map From_Process's page Address into the first free shared slot of
   --  To_Process, and set Shared to the slot's page.

   procedure Disable_Write_Pages
     (Process : in out Kernel_Process_Entry);

   procedure Enable_Write_Pages
     (Process : in out Kernel_Process_Entry);

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference
     (Layout : Rose.Capabilities.Layout.Capability_Layout)
   is
      use Rose.Kernel.Derivations;
      use type Rose.Objects.Derivation_Id;
      Id : constant Record_Id := Layout.Header.Derivation;
   begin
      if Id /= 0
        and then Is_Used (Derivation_Records, Id)
        and then Slots (Derivation_Records, Id) < Max_Slots
      then
         Add_Slot (Derivation_Records, Id);
      end if;
   end Add_Reference;

   ----------------------
   -- Assign_Object_Id --
   ----------------------

   procedure Assign_Object_Id (Pid : Process_Id) is
      use type Rose.Objects.Object_Id;
   begin
      Process_Table (Pid).Oid := Next_Object_Id;
      Next_Object_Id := Next_Object_Id + 1;
   end Assign_Object_Id;

   ------------------------
   -- Can_Send_To_Memory --
   ------------------------

   function Can_Send_To_Memory return Boolean is
      Mem : Kernel_Process_Entry renames Process_Table (Mem_Process);
   begin
      return Mem.State = Blocked
        and then (not Mem.Flags (Wait_Reply)
                  or else Mem.Flags (Receive_Any));
   end Can_Send_To_Memory;

   ----------------
   -- Cap_Layout --
   ----------------

   function Cap_Layout
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Layout
   is
   begin
      return Cap_Tables.Layout (Process_Table (Pid).Caps, Cap);
   end Cap_Layout;

   --------------
   -- Cap_Type --
   --------------

   function Cap_Type
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Type
   is
   begin
      if Is_Valid_Process_Id (Pid)
        and then Has_Cap (Pid, Cap)
      then
         return Cap_Layout (Pid, Cap).Header.Cap_Type;
      else
         return Rose.Capabilities.Layout.Null_Cap;
      end if;
   end Cap_Type;

   ------------------------
   -- Check_Buffer_Pages --
   ------------------------

   procedure Check_Buffer_Pages
     (Pid    : Process_Id;
      Params : Rose.Invocation.Invocation_Record;
      Mapped : out Boolean)
   is
      First    : constant Virtual_Address :=
                   To_Virtual_Address (Params.Buffer_Address);
      Length   : constant Virtual_Address :=
                   Virtual_Address (Params.Buffer_Length);
      Writable : constant Boolean :=
                   Params.Control.Flags (Rose.Invocation.Writable_Buffer);
   begin
      Mapped := True;

      if not Params.Control.Flags (Rose.Invocation.Send_Buffer)
        or else Length = 0
      then
         return;
      end if;

      for Page in Rose.Addresses.Virtual_Address_To_Page (First)
        .. Rose.Addresses.Virtual_Address_To_Page (First + Length - 1)
      loop
         if not Is_User_Page (Pid, Page) then
            Fault_Call (Pid, Page, Is_Mapped => False, Write => Writable);
            Mapped := False;
            return;
         elsif Writable and then not Is_Writable_User_Page (Pid, Page) then
            Fault_Call (Pid, Page, Is_Mapped => True, Write => True);
            Mapped := False;
            return;
         end if;
      end loop;
   end Check_Buffer_Pages;

   ------------------------
   -- Check_Reply_Record --
   ------------------------

   procedure Check_Reply_Record
     (Pid      : Process_Id;
      Writable : out Boolean)
   is
      use type Rose.Words.Word;
      P            : Kernel_Process_Entry renames Process_Table (Pid);
      Record_Bytes : constant Virtual_Address :=
                       Rose.Invocation.Invocation_Record'Size / 8;
      First        : constant Virtual_Address :=
                       Virtual_Address (P.Stack.EAX);

      procedure Hold (Page : Rose.Addresses.Virtual_Page_Address);
      --  Hold the reply for a write fault on Page.

      ----------
      -- Hold --
      ----------

      procedure Hold (Page : Rose.Addresses.Virtual_Page_Address) is
      begin
         Writable := False;

         if Log_Page_Faults and then Trace (Pid) then
            Debug.Put (Pid);
            Rose.Boot.Console.Put (": reply held for page ");
            Rose.Boot.Console.Put (Rose.Words.Word (Page) * 4096);
            Rose.Boot.Console.New_Line;
         end if;

         --  Reply_Fault first, so that Set_Current_State keeps
         --  Invoke_Reply.

         P.Flags (Reply_Fault) := True;
         Set_Current_State (Pid, Interrupted);

         Current_Page_Fault_Count := Current_Page_Fault_Count + 1;

         Handle_Page_Fault
           (Pid               => Pid,
            Virtual_Page      => Page,
            Is_Mapped         => Is_User_Page (Pid, Page),
            Read_Attempt      => False,
            Write_Attempt     => True,
            Execution_Attempt => False);
      end Hold;

   begin
      Writable := True;

      for Page in Rose.Addresses.Virtual_Address_To_Page (First)
        .. Rose.Addresses.Virtual_Address_To_Page (First + Record_Bytes - 1)
      loop
         if not Is_Writable_User_Page (Pid, Page) then
            Hold (Page);
            return;
         end if;
      end loop;

      for Index in 0 .. P.Lent_Buffer_Pages - 1 loop
         declare
            Page : constant Rose.Addresses.Virtual_Page_Address :=
                     P.Lent_Buffer_Page
                       + Rose.Addresses.Virtual_Page_Address (Index);
         begin
            if not Is_Writable_User_Page (Pid, Page) then
               Hold (Page);
               return;
            end if;
         end;
      end loop;

      P.Lent_Buffer_Pages := 0;
   end Check_Reply_Record;

   --------------
   -- Copy_Cap --
   --------------

   procedure Copy_Cap
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Cap             : Rose.Capabilities.Capability)
   is
      To_Cap : constant Rose.Capabilities.Capability :=
                 Create_Cap (To_Process_Id);
   begin
      --  A capability that From_Process_Id does not hold is not copied.
      --  It still takes a number in the other table, which holds nothing,
      --  since those that follow are at numbers that count on it.

      --  A single-use cap moves, as it does in a message (ADR 0032, #864).

      if Has_Cap (From_Process_Id, Cap) then
         Copy_Cap_Layout (From_Process_Id, Cap,
                          To_Process_Id, To_Cap);
         if Cap_Layout (From_Process_Id, Cap).Header.Single_Use then
            Set_Cap (From_Process_Id, Cap,
                     Rose.Capabilities.Layout.Empty_Capability);
         end if;
      end if;
   end Copy_Cap;

   ---------------------
   -- Copy_Cap_Layout --
   ---------------------

   procedure Copy_Cap_Layout
     (From_Process_Id : Process_Id;
      From_Cap        : Rose.Capabilities.Capability;
      To_Process_Id   : Process_Id;
      To_Cap          : Rose.Capabilities.Capability)
   is
   begin
      Set_Cap (To_Process_Id, To_Cap,
               Cap_Layout (From_Process_Id, From_Cap));
   end Copy_Cap_Layout;

   ----------------
   -- Create_Cap --
   ----------------

   function Create_Cap
     (Pid : Process_Id)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      Cap : Rose.Capabilities.Capability;
   begin
      Cap_Tables.Create (Process_Table (Pid).Caps, Cap);

      if Cap = Rose.Capabilities.Null_Capability then
         Debug.Put (Pid);
         Rose.Boot.Console.Put_Line (": out of capabilities");
      end if;

      return Cap;
   end Create_Cap;

   -----------------
   -- Create_Caps --
   -----------------

   procedure Create_Caps
     (Pid   : Process_Id;
      Caps  : out Rose.Capabilities.Capability_Array)
   is
      use Rose.Capabilities;
   begin
      for I in Caps'Range loop
         Caps (I) := Create_Cap (Pid);
         if Caps (I) = Null_Capability then
            for J in Caps'First .. I - 1 loop
               Delete_Cap (Pid, Caps (J));
               Caps (J) := 0;
            end loop;
            return;
         end if;
      end loop;
   end Create_Caps;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint
     (Pid : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Objects.Endpoint_Index
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      for I in P.Endpoints'Range loop
         if P.Endpoints (I).Endpoint = Rose.Objects.Null_Endpoint_Id then
            P.Endpoints (I).Endpoint := Endpoint;
            return I;
         end if;
      end loop;
      return 0;
   end Create_Endpoint;

   -------------------------
   -- Create_Endpoint_Cap --
   -------------------------

   function Create_Endpoint_Cap
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability
   is
      use Rose.Capabilities, Rose.Objects;
      Eix : constant Rose.Objects.Endpoint_Index :=
              Find_Endpoint (Pid, Endpoint);
   begin
      if Eix = 0 then
         return Null_Capability;
      end if;

      return Cap : constant Capability := Create_Cap (Pid) do
         if Cap /= Null_Capability then
            Process_Table (Pid).Endpoints (Eix).Send_Cap := Cap;
         end if;
      end return;
   end Create_Endpoint_Cap;

   --------------------------------
   -- Create_Process_Table_Entry --
   --------------------------------

   procedure Create_Process_Table_Entry
     (Pid : Rose.Kernel.Processes.Process_Id;
      Name : String)
   is
      use Rose.Kernel.Page_Table;
      Proc : Kernel_Process_Entry renames Process_Table (Pid);
      System_Call_Page : constant Physical_Page_Address :=
                                  Rose.Kernel.Heap.Get_Physical_Page
                                    (Virtual_Address_To_Page
                                       (To_Virtual_Address
                                          (System_Call_Entry'Address)));
      Directory_VP            : Virtual_Page_Address;
      Physical_Directory_Page : Physical_Page_Address;
      Proc_Name               : Process_Name := (others => ' ');
   begin

      --  Kill_Process does not release the entry of the process that is
      --  running, whose page directory is in use, so that is done here.

      if Proc.Directory_Page /= 0 then
         Release_Entry (Pid);
      end if;

      --  A new process starts with a clean FPU and SSE state.

      if FPU_Owner = Process_Table (Pid)'Access then
         FPU_Owner := null;
      end if;
      Proc.FPU_Valid := False;
      Proc.Signals := 0;
      Proc.Lent_Buffer_Pages := 0;

      Assign_Object_Id (Pid);
      Proc.Priority := Process_Priority'Last - 1;

      if Name'Length <= Proc_Name'Length then
         Proc_Name (1 .. Name'Length) := Name;
      else
         Proc_Name := Name (Name'First .. Name'First + Proc_Name'Length - 1);
      end if;

      Proc.Name := Proc_Name;

      Proc.State := Starting;
      Proc.Quantum_Ticks := 10;
      Proc.Remaining_Ticks := 10;
      Proc.Command_Endpoint := 0;
      Proc.Faults_At_Create := Current_Page_Fault_Count;

      Proc.Page_Flags := (others => (others => False));
      Proc.Page_Ranges := (others => (others => 0));

      Directory_VP := Rose.Kernel.Heap.Allocate_Page;
      Physical_Directory_Page :=
        Rose.Kernel.Heap.Get_Physical_Page (Directory_VP);

      Proc.Directory_Page :=
        Physical_Page_To_Address (Physical_Directory_Page);

      Rose.Kernel.Page_Table.Init_User_Page_Directory (Directory_VP);
      Proc.Invocation_Page := Rose.Kernel.Heap.Allocate_Page;

      Proc.Page_Ranges (Stack_Range_Index) :=
        (Virtual_Address_To_Page (Process_Stack_Bound - 16#1_0000#),
         Virtual_Address_To_Page (Process_Stack_Bound));
      Proc.Page_Flags (Stack_Range_Index) :=
        (Valid  => True, Readable => True, Writable => True,
         others => False);

      Proc.Page_Ranges (Invocation_Range_Index) :=
        (Virtual_Address_To_Page (Invocation_Buffer_Range_Base),
         Virtual_Address_To_Page (Invocation_Buffer_Range_Bound));

      Proc.Shared_Pages :=
        (1 => (Proc.Page_Ranges (Invocation_Range_Index).Base, 0),
         others => (0, 0));

      Map_Page
        (Directory_Page => Directory_VP,
         Virtual_Page   =>
           Virtual_Address_To_Page (Invocation_Buffer_Range_Base),
         Physical_Page  => Proc.Invocation_Page,
         Readable       => True,
         Writable       => True,
         Executable     => False,
         User           => True);

      Map_Page
        (Directory_Page => Directory_VP,
         Virtual_Page   =>
           Virtual_Address_To_Page (16#FFFF_0000#),
         Physical_Page  => System_Call_Page,
         Readable       => True,
         Writable       => False,
         Executable     => True,
         User           => True);

      Proc.Flags := (Persistent => True, others => False);

      declare
         use type Rose.Objects.Object_Id;
      begin
         if Proc.Oid = Trace_Object_Id then
            Proc.Flags (Trace) := True;
         end if;
      end;

      --  if Pid = 19 or else Pid = 20 or else Pid = 22 then
      --     Proc.Flags (Trace) := True;
      --  end if;

   end Create_Process_Table_Entry;

   ----------------------
   -- Current_Activity --
   ----------------------

   function Current_Activity
     (Pid : Process_Id)
      return Activity
   is
      use type Rose.Capabilities.Capability;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      if P.State /= Blocked then
         return Other;
      elsif P.Flags (Wait_Reply)
        or else P.Flags (Checkpoint_Reply_Held)
      then
         return Calling;
      elsif P.Receive_Cap /= Rose.Capabilities.Null_Capability
        and then P.Current_Params.Cap = P.Receive_Cap
      then

         --  Receive and Receive_Any set both. A process that waits for a
         --  receiver has its send cap in its current invocation instead.

         return Receiving;
      else
         return Calling;
      end if;
   end Current_Activity;

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
   is
      Layout : constant Rose.Capabilities.Layout.Capability_Layout :=
                 Cap_Tables.Layout (Process_Table (Pid).Caps, Cap);
   begin
      if Cap_Tables.Is_Active (Process_Table (Pid).Caps, Cap) then
         Drop_Reference (Layout);
      end if;

      Cap_Tables.Delete (Process_Table (Pid).Caps, Cap);
   end Delete_Cap;

   ----------------
   -- Derive_Cap --
   ----------------

   procedure Derive_Cap
     (Pid     : Process_Id;
      Cap     : Rose.Capabilities.Capability;
      New_Cap : out Rose.Capabilities.Capability;
      Error   : out Rose.Invocation.Invocation_Error)
   is
      use Rose.Kernel.Derivations;
      use type Rose.Capabilities.Capability;
      use type Rose.Objects.Derivation_Id;
      Layout    : Rose.Capabilities.Layout.Capability_Layout;
      Source_Id : Record_Id;
      New_Id    : Record_Id := 0;
   begin
      New_Cap := Rose.Capabilities.Null_Capability;
      Error   := Rose.Invocation.Invalid_Operation;

      if not Has_Cap (Pid, Cap) then
         return;
      end if;

      Layout := Cap_Layout (Pid, Cap);
      Source_Id := Layout.Header.Derivation;

      if not Is_Valid_Derivation (Layout)
        or else Depth (Derivation_Records, Source_Id) >= Max_Depth
      then
         return;
      end if;

      Error := Rose.Invocation.Out_Of_Capabilities;

      New_Cap := Create_Cap (Pid);

      if New_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Derive (Derivation_Records, Source_Id, New_Id);

      if New_Id = 0 then
         Delete_Cap (Pid, New_Cap);
         New_Cap := Rose.Capabilities.Null_Capability;
         return;
      end if;

      --  Derive counted the slot that it made the record for, and Set_Cap
      --  counts the slot that it writes.

      Layout.Header.Derivation := New_Id;
      Set_Cap (Pid, New_Cap, Layout);
      Remove_Slot (Derivation_Records, New_Id);
   end Derive_Cap;

   -------------------------
   -- Disable_Write_Pages --
   -------------------------

   procedure Disable_Write_Pages
     (Process : in out Kernel_Process_Entry)
   is
      Addr : constant Rose.Addresses.Virtual_Page_Address :=
               Rose.Kernel.Heap.Get_Virtual_Page
                 (Physical_Address_To_Page (Process.Directory_Page));
   begin
      Debug.Put (Process.Pid);
      Rose.Boot.Console.Put (": disable write pages");
      Rose.Boot.Console.New_Line;
      Rose.Kernel.Page_Table.Activate_Page_Directory (Addr);
      Rose.Kernel.Page_Table.Disable_Page_Writes (Addr);
   end Disable_Write_Pages;

   -----------------
   -- Discard_Cap --
   -----------------

   procedure Discard_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
   is
      use type Rose.Capabilities.Layout.Capability_Type;
   begin
      if Cap_Tables.Is_Active (Process_Table (Pid).Caps, Cap) then
         declare
            Layout : constant Rose.Capabilities.Layout.Capability_Layout :=
              Cap_Layout (Pid, Cap);
         begin
            if Layout.Header.Cap_Type = Rose.Capabilities.Layout.Reply_Cap
            then
               declare
                  Caller : constant Process_Id :=
                    To_Process_Id (Layout.Payload);
               begin
                  if Caller /= Null_Process_Id
                    and then Caller /= Pid
                    and then Process_Table (Caller).State = Blocked
                    and then Process_Table (Caller).Flags (Wait_Reply)
                  then
                     Wake_With_Error (Caller, Rose.Invocation.Closed);

                     --  The server that received the call may still have
                     --  the caller's buffer mapped, and goes on running,
                     --  unlike one that is killed. A caller has one call
                     --  at a time, so every slot that names it is this
                     --  call's: free them, so that the server can't write
                     --  pages that the caller now uses for something
                     --  else, and doesn't run out of slots.

                     for Server in Process_Table'Range loop
                        if Server /= Caller
                          and then Process_Table (Server).State
                            not in Available | Killed
                        then
                           Unmap_Invocation_Buffer (Server, Caller);
                        end if;
                     end loop;
                  end if;
               end;
            end if;
         end;
      end if;

      Delete_Cap (Pid, Cap);
   end Discard_Cap;

   --------------------
   -- Drop_Reference --
   --------------------

   procedure Drop_Reference
     (Layout : Rose.Capabilities.Layout.Capability_Layout)
   is
      use Rose.Kernel.Derivations;
      use type Rose.Objects.Derivation_Id;
      Id : constant Record_Id := Layout.Header.Derivation;
   begin
      if Id /= 0
        and then Is_Used (Derivation_Records, Id)
        and then Slots (Derivation_Records, Id) > 0
      then
         Remove_Slot (Derivation_Records, Id);
      end if;
   end Drop_Reference;

   ------------------------
   -- Enable_Write_Pages --
   ------------------------

   procedure Enable_Write_Pages
     (Process : in out Kernel_Process_Entry)
   is
      Addr : constant Rose.Addresses.Virtual_Page_Address :=
               Rose.Kernel.Heap.Get_Virtual_Page
                 (Physical_Address_To_Page (Process.Directory_Page));
   begin
      Debug.Put (Process.Pid);
      Rose.Boot.Console.Put (": enable write pages");
      Rose.Boot.Console.New_Line;
      Rose.Kernel.Page_Table.Activate_Page_Directory (Addr);
      Rose.Kernel.Page_Table.Enable_Page_Writes (Addr);
   end Enable_Write_Pages;

   ----------------------
   -- Enter_Checkpoint --
   ----------------------

   procedure Enter_Checkpoint is
   begin
      Active_Checkpoint := True;
      for P of Process_Table.all loop
         if P.State /= Available
           and then P.Flags (Persistent)
         then
            Disable_Write_Pages (P);
         end if;
      end loop;
   end Enter_Checkpoint;

   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process (Pid : Process_Id) is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      if not Have_Process_Handlers
        or else not P.Flags (Memory_Registered)
      then
         Kill_Process (Pid);
         return;
      end if;

      --  The entry stays until mem kills the process, so it is not used
      --  again before mem has forgotten it.

      Wake_Waiting_Processes (Pid);
      Set_Current_State (Pid, Killed);

      if Can_Send_To_Memory then
         Send_Exit (Pid);
      else
         P.Flags (Exit_Pending) := True;
      end if;
   end Exit_Process;

   -----------------
   -- Expand_Heap --
   -----------------

   procedure Expand_Heap
     (Amount : Rose.Addresses.Physical_Bytes)
   is
   begin
      if Heap_Request_Pending then
         return;
      end if;

      Heap_Request_Amount := Amount;
      if Can_Send_To_Memory then
         Send_Heap_Request;
      else
         Heap_Request_Pending := True;
      end if;
   end Expand_Heap;

   ----------------
   -- Fault_Call --
   ----------------

   procedure Fault_Call
     (Pid          : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address;
      Is_Mapped    : Boolean;
      Write        : Boolean)
   is
      use type Rose.Words.Word;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      if Log_Page_Faults and then Trace (Pid) then
         Debug.Put (Pid);
         Rose.Boot.Console.Put (": call faults on buffer page ");
         Rose.Boot.Console.Put (Rose.Words.Word (Virtual_Page) * 4096);
         Rose.Boot.Console.New_Line;
      end if;

      --  Interrupted, as for a fault in user mode, so that mem's resume
      --  and a checkpoint treat it the same way; Call_Restart makes it
      --  return by making the call again.

      Set_Current_State (Pid, Interrupted);
      Restart_Call (P);
      P.Flags (Call_Restart) := True;

      Current_Page_Fault_Count := Current_Page_Fault_Count + 1;

      Handle_Page_Fault
        (Pid               => Pid,
         Virtual_Page      => Virtual_Page,
         Is_Mapped         => Is_Mapped,
         Read_Attempt      => not Write,
         Write_Attempt     => Write,
         Execution_Attempt => False);
   end Fault_Call;

   -------------------
   -- Find_Endpoint --
   -------------------

   function Find_Endpoint
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Objects.Endpoint_Index
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      for Id in P.Endpoints'Range loop
         if P.Endpoints (Id).Endpoint = Endpoint then
            return Id;
         end if;
      end loop;
      return 0;
   end Find_Endpoint;

   -----------------------
   -- Find_Endpoint_Cap --
   -----------------------

   function Find_Endpoint_Cap
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      for Rec of P.Endpoints loop
         if Rec.Endpoint = Endpoint then
            return Rec.Send_Cap;
         end if;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Find_Endpoint_Cap;

   ----------------------
   -- Find_Receive_Cap --
   ----------------------

   function Find_Receive_Cap
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      for Rec of P.Endpoints loop
         if Rec.Endpoint = Endpoint then
            return Rec.Receive_Cap;
         end if;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Find_Receive_Cap;

   -------------
   -- Get_Cap --
   -------------

   procedure Get_Cap
     (Pid    : Process_Id;
      Cap    : Rose.Capabilities.Capability;
      Layout : out Rose.Capabilities.Layout.Capability_Layout)
   is
   begin
      Layout := Cap_Tables.Layout (Process_Table (Pid).Caps, Cap);
   end Get_Cap;

   ----------------------------
   -- Get_Current_Invocation --
   ----------------------------

   procedure Get_Current_Invocation
     (Pid        : Process_Id;
      Invocation : out Rose.Invocation.Invocation_Record)
   is
   begin
      Invocation := Process_Table (Pid).Current_Params;
   end Get_Current_Invocation;

   ----------------------
   -- Get_Process_Name --
   ----------------------

   procedure Get_Process_Name
     (Pid  : Process_Id;
      Name : out String;
      Last : out Natural)
   is
   begin
      Last := Name'First - 1;
      for Ch of Process_Table (Pid).Name loop
         exit when Ch = ' ' or else Last >= Name'Last;
         Last := Last + 1;
         Name (Last) := Ch;
      end loop;
   end Get_Process_Name;

   -------------------------------------
   -- Handle_General_Protection_Fault --
   -------------------------------------

   function Handle_General_Protection_Fault
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status
   is
   begin
      Debug.Put (Current_Process_Id);
      Rose.Boot.Console.Put_Line (": general protection fault");
      Debug.Report_Process (Current_Process_Id);
      Rose.Boot.Console.Put_Line ("mapped page report");
      Rose.Kernel.Page_Table.Report_Mapped_Pages
        (Current_Process.Directory_Page);

      Rose.Boot.Console.Put_Line ("Faulted");
      Set_Current_State (Current_Process_Id, Killed);
      return Rose.Kernel.Interrupts.Not_Finished;
   end Handle_General_Protection_Fault;

   -----------------------
   -- Handle_Page_Fault --
   -----------------------

   function Handle_Page_Fault
     (Code : Rose.Words.Word)
      return Rose.Kernel.Interrupts.Interrupt_Handler_Status
   is
      use type Rose.Words.Word;
      Virtual_Address : constant Rose.Addresses.Virtual_Address :=
                          Rose.Addresses.Virtual_Address (Page_Fault_Address);
      Virtual_Page    : constant Rose.Addresses.Virtual_Page_Address :=
                          Rose.Addresses.Virtual_Address_To_Page
                            (Virtual_Address);
      User_Mode            : constant Boolean := (Code and 4) /= 0;
      Protection_Violation : constant Boolean := (Code and 1) /= 0;
      Write_Attempt        : constant Boolean := (Code and 2) /= 0;
      Execution_Attempt    : constant Boolean := (Code and 16) /= 0;
   begin

      if not User_Mode then
         Rose.Kernel.Panic.Panic
           (Message => "page fault in supervisor mode",
            Addr    => Virtual_Address);
      end if;

      if Log_Page_Faults and then Trace (Current_Process_Id) then
         Rose.Boot.Console.Put ("page-fault: ");
         Rose.Boot.Console.Put ("pid ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Current_Process_Id));
         Rose.Boot.Console.Put ("; address ");
         Rose.Boot.Console.Put (Rose.Words.Word (Virtual_Address));
         Rose.Boot.Console.Put ("; flags ");
         Rose.Boot.Console.Put (if Execution_Attempt then "x" else "-");
         Rose.Boot.Console.Put (if User_Mode then "u" else "-");
         Rose.Boot.Console.Put (if Write_Attempt then "w" else "-");
         Rose.Boot.Console.Put (if Protection_Violation then "p" else "-");
         Rose.Boot.Console.New_Line;
         Debug.Report_Process (Current_Process_Id);
      end if;

      Current_Page_Fault_Count := Current_Page_Fault_Count + 1;

      Handle_Page_Fault
        (Pid               => Current_Process_Id,
         Virtual_Page      => Virtual_Page,
         Is_Mapped         => Protection_Violation,
         Read_Attempt      => not Write_Attempt and then not Execution_Attempt,
         Write_Attempt     => Write_Attempt,
         Execution_Attempt => Execution_Attempt);

      return Rose.Kernel.Interrupts.Not_Finished;
   end Handle_Page_Fault;

   -----------------------
   -- Handle_Page_Fault --
   -----------------------

   procedure Handle_Page_Fault
     (Pid               : Process_Id;
      Virtual_Page      : Rose.Addresses.Virtual_Page_Address;
      Is_Mapped         : Boolean;
      Read_Attempt      : Boolean;
      Write_Attempt     : Boolean;
      Execution_Attempt : Boolean)
   is
      pragma Unreferenced (Read_Attempt);
      use type Rose.Words.Word;
   begin

      if Active_Checkpoint
        and then Is_Mapped
        and then Write_Attempt
        and then Process_Table (Pid).Flags (Persistent)
      then
         --  write attempt failed because of active checkpoint.
         --  record situation and return
         --
         --  Only a persistent process has its pages made read-only by a
         --  checkpoint, and Leave_Checkpoint only resumes those, so any
         --  other write fault goes to mem as usual (#231).

         Debug.Put (Pid);
         Rose.Boot.Console.Put (": checkpoint write blocked");
         Rose.Boot.Console.New_Line;
         Rose.Boot.Console.Put ("virtual: ");
         Rose.Boot.Console.Put (Rose.Words.Word (Virtual_Page));
         Rose.Boot.Console.New_Line;
         Debug.Report_Process (Pid);

         declare
            Proc : Kernel_Process_Entry renames
                     Process_Table (Pid);
         begin
            Proc.Flags (Checkpoint_Page_Fault) := True;
            Proc.Checkpoint_Fault := Virtual_Page;
         end;

         return;
      end if;

      if Pid = Mem_Process then
         Rose.Boot.Console.Put ("virtual ");
         Rose.Boot.Console.Put (Rose.Words.Word (Virtual_Page) * 4096);
         Rose.Boot.Console.New_Line;
         Rose.Kernel.Processes.Debug.Report_Process (Pid);
         Rose.Kernel.Panic.Panic
           ("page fault in page fault handler process");
      end if;

      declare
         Proc : Kernel_Process_Entry renames Process_Table (Pid);
      begin
         Proc.Pending_Fault_Page := Virtual_Page;
         Proc.Pending_Fault_Mapped := Is_Mapped;
         Proc.Pending_Fault_Write := Write_Attempt;
         Proc.Pending_Fault_Execute := Execution_Attempt;
      end;

      --  The process stays interrupted until mem resumes it, so a fault
      --  that has to wait for mem to receive is only marked.

      if Can_Send_To_Memory then
         Send_Page_Fault (Pid);
      else
         Process_Table (Pid).Flags (Fault_Pending) := True;
      end if;

   end Handle_Page_Fault;

   --------------------------
   -- Handover_In_Progress --
   --------------------------

   function Handover_In_Progress return Boolean is
   begin
      for Handover of Handovers loop
         if Handover.Held or else Handover.Retired then
            return True;
         end if;
      end loop;
      return False;
   end Handover_In_Progress;

   -------------
   -- Has_Cap --
   -------------

   function Has_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Boolean
   is
      use Rose.Capabilities.Layout;
      Layout : Capability_Layout;
   begin
      if not Cap_Tables.Is_Active (Process_Table (Pid).Caps, Cap) then
         return False;
      else
         Get_Cap (Pid, Cap, Layout);
         return Layout.Header.Cap_Type /= Null_Cap;
      end if;
   end Has_Cap;

   ------------------------
   -- Has_Checked_Record --
   ------------------------

   function Has_Checked_Record
     (Pid     : Process_Id;
      Address : Rose.Words.Word_32)
      return Boolean
   is
      use type Rose.Kernel.Page_Table.Change_Count;
      use type Rose.Words.Word_32;
      Checked : Checked_Record renames Checked_Records (Pid);
   begin
      return Checked.Valid
        and then Checked.Address = Address
        and then Checked.Changes = Rose.Kernel.Page_Table.Changes;
   end Has_Checked_Record;

   --------------------------------
   -- Has_Pending_Memory_Request --
   --------------------------------

   function Has_Pending_Memory_Request
     (Receiver : Process_Id)
      return Boolean
   is
   begin
      if Receiver /= Mem_Process then
         return False;
      elsif Heap_Request_Pending then
         return True;
      end if;

      for P of Process_Table.all loop
         if P.Flags (Fault_Pending) or else P.Flags (Exit_Pending) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Pending_Memory_Request;

   -------------------------
   -- Has_Pending_Signals --
   -------------------------

   function Has_Pending_Signals
     (Process  : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      use type Rose.Objects.Endpoint_Index;
      use type Rose.Words.Word_32;
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      if P.Signals = 0 then
         return False;
      end if;

      declare
         Signal_Endpoint : constant Rose.Objects.Endpoint_Index :=
                             Find_Endpoint
                               (Process,
                                Rose.Interfaces.Signalled.Signal_Endpoint);
      begin
         return Signal_Endpoint /= 0
           and then (Endpoint = 0 or else Endpoint = Signal_Endpoint);
      end;
   end Has_Pending_Signals;

   ------------------------
   -- Has_Queued_Message --
   ------------------------

   function Has_Queued_Message
     (Process     : Process_Id;
      Endpoint    : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      use type Rose.Objects.Endpoint_Index;
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      return P.Flags (Message_Queued)
        and then (Endpoint = 0
                  or else Endpoint = P.Queued_Endpoint);
   end Has_Queued_Message;

   ------------------------
   -- Has_Restore_Notice --
   ------------------------

   function Has_Restore_Notice
     (Process  : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      use type Rose.Objects.Endpoint_Index;
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      return P.Flags (Restore_Notice)
        and then (Endpoint = 0
                  or else Endpoint =
                    Find_Endpoint
                      (Process,
                       Rose.Interfaces.Restorable.Restored_Endpoint));
   end Has_Restore_Notice;

   ---------------------------
   -- Have_Process_Handlers --
   ---------------------------

   function Have_Process_Handlers return Boolean is
   begin
      return Mem_Process /= 0;
   end Have_Process_Handlers;

   ----------------
   -- Hold_Calls --
   ----------------

   procedure Hold_Calls
     (Pid    : Process_Id;
      Except : Rose.Objects.Endpoint_Id)
   is
      use type Rose.Objects.Endpoint_Id;
   begin

      --  Find_Endpoint would find a free slot for the null id.

      Handovers (Pid).Held := True;
      Handovers (Pid).Except :=
        (if Except = Rose.Objects.Null_Endpoint_Id then 0
         else Find_Endpoint (Pid, Except));
   end Hold_Calls;

   ----------------------------
   -- Is_Blocked_On_Endpoint --
   ----------------------------

   function Is_Blocked_On_Endpoint
     (Pid            : Process_Id;
      Endpoint_Index : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      if P.State /= Blocked then
         return False;
      end if;

      if P.Flags (Receive_Any) then
         return True;
      end if;

      if not Has_Cap (Pid, P.Receive_Cap) then
         return False;
      end if;

      if P.Flags (Wait_Reply) then
         return False;
      end if;

      declare
         use Rose.Objects;
         use type Rose.Capabilities.Layout.Capability_Type;
         Rec : Rose.Capabilities.Layout.Capability_Layout;
         Log : constant Boolean := False;
      begin
         Get_Cap (Pid, P.Receive_Cap, Rec);
         if Rec.Header.Cap_Type /= Rose.Capabilities.Layout.Receive_Cap then
            return False;
         end if;

         declare
            Endpoint : constant Rose.Objects.Endpoint_Index :=
                         Rec.Header.Endpoint;
            Blocked  : constant Boolean :=
                         Endpoint = 0
                             or else Endpoint = Endpoint_Index;
         begin
            if not Blocked then
               if Log then
                  Debug.Put (Current_Process_Id);
                  Rose.Boot.Console.Put (": receiver ");
                  Debug.Put (Pid);
                  Rose.Boot.Console.Put (" is blocked on ");
                  Rose.Boot.Console.Put
                    (Rose.Words.Word_8
                       (Rec.Header.Endpoint));
                  Rose.Boot.Console.Put (" ");
                  Rose.Boot.Console.Put
                    (Rose.Words.Word_64
                       (P.Endpoints (Rec.Header.Endpoint).Endpoint));
                  Rose.Boot.Console.New_Line;

                  Rose.Boot.Console.Put (" but we are sending to ");
                  Rose.Boot.Console.Put
                    (Rose.Words.Word_8
                       (Endpoint_Index));
                  Rose.Boot.Console.Put (" ");
                  Rose.Boot.Console.Put
                    (Rose.Words.Word_64
                       (P.Endpoints (Endpoint_Index).Endpoint));
                  Rose.Boot.Console.New_Line;
               end if;
            end if;

            return Blocked;
         end;
      end;
   end Is_Blocked_On_Endpoint;

   ----------------
   -- Is_Command --
   ----------------

   function Is_Command (Pid : Process_Id) return Boolean is
      use type Rose.Objects.Endpoint_Index;
   begin
      return Process_Table (Pid).Command_Endpoint /= 0;
   end Is_Command;

   -------------------------
   -- Is_Command_Endpoint --
   -------------------------

   function Is_Command_Endpoint
     (Pid      : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      use type Rose.Objects.Endpoint_Index;
   begin
      return Endpoint /= 0
        and then Process_Table (Pid).Command_Endpoint = Endpoint;
   end Is_Command_Endpoint;

   -------------
   -- Is_Held --
   -------------

   function Is_Held
     (Pid      : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      use type Rose.Objects.Endpoint_Index;
   begin
      return Handovers (Pid).Held
        and then Endpoint /= Handovers (Pid).Except;
   end Is_Held;

   ----------------------------
   -- Is_Repeated_Single_Use --
   ----------------------------

   function Is_Repeated_Single_Use
     (Pid       : Process_Id;
      Params    : Rose.Invocation.Invocation_Record;
      Cap_Index : Rose.Invocation.Capability_Index)
      return Boolean
   is
      use type Rose.Capabilities.Capability;
      use type Rose.Invocation.Capability_Index;
      Cap : constant Rose.Capabilities.Capability := Params.Caps (Cap_Index);
   begin
      if Cap_Index = 0 or else not Cap_Layout (Pid, Cap).Header.Single_Use
      then
         return False;
      end if;

      for Earlier in 0 .. Cap_Index - 1 loop
         if Params.Caps (Earlier) = Cap then
            return True;
         end if;
      end loop;

      return False;
   end Is_Repeated_Single_Use;

   ----------------
   -- Is_Retired --
   ----------------

   function Is_Retired
     (Pid : Process_Id)
      return Boolean
   is
   begin
      return Handovers (Pid).Retired;
   end Is_Retired;

   ------------------
   -- Is_User_Page --
   ------------------

   function Is_User_Page
     (Pid          : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Boolean
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      return Rose.Kernel.Page_Table.Is_User_Page
        (Rose.Kernel.Heap.Get_Virtual_Page
           (Physical_Address_To_Page (P.Directory_Page)),
         Virtual_Page);
   end Is_User_Page;

   -----------------------------
   -- Is_Valid_Derivation --
   -------------------------

   function Is_Valid_Derivation
     (Layout : Rose.Capabilities.Layout.Capability_Layout)
      return Boolean
   is
      use Rose.Kernel.Derivations;
      use type Rose.Objects.Derivation_Id;
      Id : constant Record_Id := Layout.Header.Derivation;
   begin
      return Id = 0
        or else (Is_Used (Derivation_Records, Id)
                 and then Is_Valid (Derivation_Records, Id));
   end Is_Valid_Derivation;

   -----------------------------
   -- Is_Valid_Endpoint_Index --
   -----------------------------

   function Is_Valid_Endpoint_Index
     (Pid            : Process_Id;
      Endpoint_Index : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      return Endpoint_Index in P.Endpoints'Range
        and then P.Endpoints (Endpoint_Index).Endpoint
        /= Rose.Objects.Null_Endpoint_Id;
   end Is_Valid_Endpoint_Index;

   --------------------
   -- Is_Valid_Entry --
   --------------------

   function Is_Valid_Entry
     (Source_Cap          : Rose.Capabilities.Layout.Capability_Layout;
      Destination_Process : Process_Id)
      return Boolean
   is
      use type Rose.Objects.Endpoint_Id;
      To   : Kernel_Process_Entry renames Process_Table (Destination_Process);
      Endpoint : constant Rose.Objects.Endpoint_Index :=
                   Source_Cap.Header.Endpoint;
   begin
      if Endpoint in To.Endpoints'Range
        and then To.Endpoints (Endpoint).Endpoint /= 0
      then
         declare
            Send_Cap : constant Rose.Capabilities.Capability :=
                         To.Endpoints (Endpoint).Send_Cap;
         begin
            return Cap_Tables.Same_Count
              (Source_Cap, Cap_Layout (Destination_Process, Send_Cap))
              and then Is_Valid_Derivation (Source_Cap);
         end;
      end if;
      return False;
   end Is_Valid_Entry;

   ---------------------------
   -- Is_Writable_User_Page --
   ---------------------------

   function Is_Writable_User_Page
     (Pid          : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Boolean
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      return Rose.Kernel.Page_Table.Is_Writable_User_Page
        (Rose.Kernel.Heap.Get_Virtual_Page
           (Physical_Address_To_Page (P.Directory_Page)),
         Virtual_Page);
   end Is_Writable_User_Page;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process
     (Process : Process_Id)
   is
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      if Trace_Launch then
         Debug.Put (Process);
         Rose.Boot.Console.Put (": trace: exit; page faults since created: ");
         Rose.Boot.Console.Put (Current_Page_Fault_Count - P.Faults_At_Create);
         Rose.Boot.Console.New_Line;
      end if;

      Wake_Waiting_Processes (Process);
      Handovers (Process) := (others => <>);

      if Process = Current_Process_Id then
         P.State := Available;
      else
         Release_Entry (Process);
      end if;
   end Kill_Process;

   ----------------------
   -- Leave_Checkpoint --
   ----------------------

   procedure Leave_Checkpoint is
   begin
      Active_Checkpoint := False;
      for P of Process_Table.all loop
         if P.State /= Available
           and then P.Flags (Persistent)
         then
            Enable_Write_Pages (P);
            if P.State = Interrupted
              and then P.Flags (Checkpoint_Page_Fault)
            then
               Debug.Put (To_Process_Id (P.Oid));
               Rose.Boot.Console.Put_Line (": resuming after checkpoint");
               P.Flags (Checkpoint_Page_Fault) := False;
               Handle_Page_Fault
                 (Pid               => P.Pid,
                  Virtual_Page      => P.Checkpoint_Fault,
                  Is_Mapped         => True,
                  Read_Attempt      => False,
                  Write_Attempt     => True,
                  Execution_Attempt => False);
            end if;
            if P.State = Blocked
              and then P.Flags (Checkpoint_Reply_Held)
            then
               Debug.Put (To_Process_Id (P.Oid));
               Rose.Boot.Console.Put_Line (": reply after checkpoint");
               P.Flags (Checkpoint_Reply_Held) := False;
               P.State := Ready;
            end if;
         end if;
      end loop;
   end Leave_Checkpoint;

   -------------------
   -- Load_Cap_Page --
   -------------------

   procedure Load_Cap_Page
     (Pid  : Process_Id;
      Page : Cap_Tables.Capability_Page_Index)
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      if not Cap_Tables.Has_Page (P.Caps, Page) then
         declare
            Page_Address : constant Virtual_Page_Address :=
                             Rose.Kernel.Heap.Allocate_Page;
         begin
            Cap_Tables.Set_Page
              (P.Caps, Page,
               System'To_Address (Virtual_Page_To_Address (Page_Address)));
         end;
      end if;
   end Load_Cap_Page;

   --------------
   -- Map_Page --
   --------------

   procedure Map_Page
     (Pid           : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      User          : Boolean;
      Managed       : Boolean := False)
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      Rose.Kernel.Page_Table.Map_Page
        (Rose.Kernel.Heap.Get_Virtual_Page
           (Physical_Address_To_Page (P.Directory_Page)),
         Virtual_Page   => Virtual_Page,
         Physical_Page  => Physical_Page,
         Readable       => Readable,
         Writable       => Writable,
         Executable     => Executable,
         User           => User,
         Managed        => Managed);
   end Map_Page;

   --------------------------
   -- Mapped_Physical_Page --
   --------------------------

   function Mapped_Physical_Page
     (Pid          : Process_Id;
      Virtual_Page : Rose.Addresses.Virtual_Page_Address)
      return Rose.Addresses.Physical_Page_Address
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      return Rose.Kernel.Page_Table.Mapped_Physical_Page
        (Directory_Page =>
           Rose.Kernel.Heap.Get_Virtual_Page
             (Physical_Address_To_Page (P.Directory_Page)),
         Virtual_Page   => Virtual_Page);
   end Mapped_Physical_Page;

   --------------------------
   -- Move_Single_Use_Caps --
   --------------------------

   procedure Move_Single_Use_Caps
     (Pid    : Process_Id;
      Params : Rose.Invocation.Invocation_Record)
   is
      use type Rose.Capabilities.Capability;
      Cap : Rose.Capabilities.Capability;
   begin
      for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
         Cap := Params.Caps (Cap_Index);
         if Cap /= Rose.Capabilities.Null_Capability
           and then Has_Cap (Pid, Cap)
           and then Cap_Layout (Pid, Cap).Header.Single_Use
         then
            Set_Cap (Pid, Cap, Rose.Capabilities.Layout.Empty_Capability);
         end if;
      end loop;
   end Move_Single_Use_Caps;

   -------------
   -- New_Cap --
   -------------

   function New_Cap
     (For_Process_Id : Process_Id;
      Layout         : Rose.Capabilities.Layout.Capability_Layout)
      return Rose.Capabilities.Capability
   is
   begin
      return Cap : constant Rose.Capabilities.Capability :=
        Create_Cap (For_Process_Id)
      do
         Set_Cap (For_Process_Id, Cap, Layout);
      end return;
   end New_Cap;

   -----------------
   -- New_Process --
   -----------------

   function New_Process
     (Name : String)
     return Process_Id
   is
      function Next (Pid : Process_Id) return Process_Id
      is (if Pid = Process_Table'Last then Process_Table'First
          else Pid + 1);

      Pid : Process_Id := Next_Pid;
   begin

      --  Look at each entry once, from where the last search stopped, so
      --  that entries freed by processes that have gone are used again.

      while Process_Table (Pid).State /= Available loop
         Pid := Next (Pid);
         if Pid = Next_Pid then
            return Null_Process_Id;
         end if;
      end loop;

      Create_Process_Table_Entry (Pid, Name);

      Next_Pid := Next (Pid);
      return Pid;

   end New_Process;

   -------------------------
   -- Next_Blocked_Sender --
   -------------------------

   function Next_Blocked_Sender
     (Receiver_Id : Process_Id;
      Endpoint    : Rose.Objects.Endpoint_Index)
      return Process_Id
   is
      use type Rose.Objects.Endpoint_Index;
   begin
      loop
         declare
            It     : Kernel_Process_Access :=
                       Process_Table (Receiver_Id).Waiting_First;
            Prev   : Kernel_Process_Access := null;
            Mapped : Boolean;
         begin

            --  While its calls are held, a process takes only the calls
            --  to the endpoint that the hold excepts (Hold_Calls).

            while It /= null
              and then ((Endpoint /= 0
                         and then It.Waiting_Endpoint /= Endpoint)
                        or else Is_Held (Receiver_Id, It.Waiting_Endpoint))
            loop
               Prev := It;
               It := It.Waiting_Next;
            end loop;

            if It = null then
               return Null_Process_Id;
            end if;

            if Prev = null then
               Process_Table (Receiver_Id).Waiting_First := It.Waiting_Next;
            else
               Prev.Waiting_Next := It.Waiting_Next;
            end if;
            It.Waiting_Next := null;

            Check_Buffer_Pages (It.Pid, It.Current_Params, Mapped);

            if Mapped then
               return It.Pid;
            end if;

            --  The call is made again, so the reply cap that it made
            --  goes (Rose.Kernel.Capabilities.Endpoint). Only an endpoint
            --  call waits for a receiver, and endpoint caps are never
            --  single use, so the call can be made again.

            if It.Current_Params.Control.Flags
              (Rose.Invocation.Create_Reply_Cap)
            then
               Delete_Cap (Receiver_Id, It.Current_Params.Reply_Cap);
            end if;
         end;
      end loop;
   end Next_Blocked_Sender;

   ----------------------
   -- Page_Fault_Count --
   ----------------------

   function Page_Fault_Count
     return Natural
   is
   begin
      return Current_Page_Fault_Count;
   end Page_Fault_Count;

   ------------------
   -- Raise_Signal --
   ------------------

   procedure Raise_Signal
     (Process : Process_Id;
      Bit     : Own_Signal)
   is
      use type Rose.Objects.Endpoint_Index;
      use type Rose.Words.Word_32;
      P               : Kernel_Process_Entry renames Process_Table (Process);
      Signal_Endpoint : constant Rose.Objects.Endpoint_Index :=
                          Find_Endpoint
                            (Process,
                             Rose.Interfaces.Signalled.Signal_Endpoint);
   begin
      P.Signals := P.Signals or 2 ** Bit;

      --  A process that is waiting to receive is told now. One that is
      --  waiting for a reply, or that has a restore notice to come
      --  first, is told when it next receives.

      if Signal_Endpoint /= 0
        and then not P.Flags (Wait_Reply)
        and then not P.Flags (Restore_Notice)
        and then Is_Blocked_On_Endpoint (Process, Signal_Endpoint)
      then
         Send_Signals (Process);
      end if;
   end Raise_Signal;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Receiver_Id : Process_Id;
      Params      : Rose.Invocation.Invocation_Record)
   is
      P : Kernel_Process_Entry renames Process_Table (Receiver_Id);
   begin
      P.Flags (Receive_Any) := False;
      P.Current_Params := Params;
      P.Receive_Cap := Params.Cap;

      Set_Current_State (Receiver_Id, Blocked);

   end Receive;

   -----------------
   -- Receive_Any --
   -----------------

   procedure Receive_Any
     (Receiver_Id : Process_Id;
      Params      : Rose.Invocation.Invocation_Record)
   is
   begin
      Receive (Receiver_Id, Params);
      Process_Table (Receiver_Id).Flags (Receive_Any) := True;
   end Receive_Any;

   -------------------
   -- Release_Calls --
   -------------------

   procedure Release_Calls
     (Pid : Process_Id)
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      Handovers (Pid).Held := False;
      Handovers (Pid).Except := 0;

      --  A process that is not waiting to receive takes the calls that
      --  wait when it next receives, as it would after any busy spell.

      if Current_Activity (Pid) /= Receiving then
         return;
      end if;

      declare
         Endpoint : constant Rose.Objects.Endpoint_Index :=
                      (if P.Flags (Receive_Any) then 0
                       else Cap_Layout (Pid, P.Receive_Cap).Header.Endpoint);
         Sender   : constant Process_Id :=
                      Next_Blocked_Sender (Pid, Endpoint);
      begin
         if Sender /= Null_Process_Id then
            declare
               S : Kernel_Process_Entry renames Process_Table (Sender);
            begin
               Send_To_Endpoint
                 (From_Process_Id => Sender,
                  To_Process_Id   => Pid,
                  Sender_Cap      => S.Current_Params.Cap,
                  Endpoint        => S.Waiting_Endpoint,
                  Identifier      => S.Waiting_Cap_Id,
                  Params          => S.Current_Params);
            end;
         end if;
      end;
   end Release_Calls;

   -------------------
   -- Release_Entry --
   -------------------

   procedure Release_Entry (Pid : Process_Id) is
      use System.Storage_Elements;
      use type System.Address;
      P   : Kernel_Process_Entry renames Process_Table (Pid);
      Me  : constant Kernel_Process_Access := P'Access;
   begin
      Rose.Kernel.Processes.Queue.Remove_Process (Pid);
      Handovers (Pid) := (others => <>);

      --  A process that went while it waited to send is still on its
      --  receiver's list.

      for Receiver of Process_Table.all loop
         if Receiver.Waiting_First = Me then
            Receiver.Waiting_First := Me.Waiting_Next;
         else
            declare
               It : Kernel_Process_Access := Receiver.Waiting_First;
            begin
               while It /= null and then It.Waiting_Next /= Me loop
                  It := It.Waiting_Next;
               end loop;
               if It /= null then
                  It.Waiting_Next := Me.Waiting_Next;
               end if;
            end;
         end if;
      end loop;

      for Cap in 1 .. Cap_Tables.Last_Cap (P.Caps) loop
         if Cap_Tables.Is_Active (P.Caps, Cap) then
            Drop_Reference (Cap_Tables.Layout (P.Caps, Cap));
         end if;
      end loop;

      for Page in Cap_Tables.Capability_Page_Index loop
         if Cap_Tables.Has_Page (P.Caps, Page) then
            Rose.Kernel.Heap.Free_Page
              (Virtual_Address_To_Page
                 (To_Virtual_Address
                    (Cap_Tables.Page_Address (P.Caps, Page))));
         end if;
      end loop;

      if P.Directory_Page /= 0 then
         declare
            Directory_VP : constant Virtual_Page_Address :=
              Rose.Kernel.Heap.Get_Virtual_Page
                (Physical_Address_To_Page (P.Directory_Page));
         begin
            Rose.Kernel.Page_Table.Free_Process_Page_Tables (Directory_VP);
            Rose.Kernel.Heap.Free_Page (Directory_VP);
         end;
      end if;

      if P.Invocation_Page /= 0 then
         Rose.Kernel.Heap.Free_Page
           (Rose.Kernel.Heap.Get_Virtual_Page (P.Invocation_Page));
      end if;

      --  The process table starts out zeroed, as the kernel heap gives
      --  it, and an entry is used as if it were new.

      declare
         Contents : Storage_Array
           (1 .. Storage_Offset (Kernel_Process_Entry'Size / 8))
           with Import, Address => P'Address;
      begin
         Contents := (others => 0);
      end;

      P.Pid := Pid;
      P.State := Available;
      P.Name := (others => ' ');
   end Release_Entry;

   ----------------------------
   -- Report_Current_Process --
   ----------------------------

   procedure Report_Current_Process is
   begin
      Rose.Kernel.Processes.Debug.Report_Process
        (Current_Process.Pid);
   end Report_Current_Process;

   ----------------------
   -- Require_Endpoint --
   ----------------------

   function Require_Endpoint
     (Pid        : Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Objects.Endpoint_Index
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      for Id in P.Endpoints'Range loop
         if P.Endpoints (Id).Endpoint = Endpoint then
            return Id;
         end if;
      end loop;
      return Create_Endpoint (Pid, Endpoint);
   end Require_Endpoint;

   -----------------
   -- Rescind_Cap --
   -----------------

   procedure Rescind_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
   is
   begin
      if Cap in 1 .. Cap_Tables.Max_Capability_Index then
         Load_Cap_Page (Pid, Cap_Tables.Page_Index (Cap));
      end if;

      declare
         use Rose.Kernel.Derivations;
         use type Rose.Objects.Derivation_Id;
         Layout : Rose.Capabilities.Layout.Capability_Layout :=
                    Cap_Tables.Layout (Process_Table (Pid).Caps, Cap);
         Old_Id : constant Record_Id := Layout.Header.Derivation;
         New_Id : Record_Id := 0;
      begin
         if Old_Id = 0
           or else not Cap_Tables.Is_Active (Process_Table (Pid).Caps, Cap)
           or else not Is_Used (Derivation_Records, Old_Id)
         then
            Cap_Tables.Rescind (Process_Table (Pid).Caps, Cap);
            return;
         end if;

         --  A derived capability is revoked by revoking its record, which
         --  makes it, what was copied from it, and what was derived from
         --  it invalid.  The capability itself goes on to a record of its
         --  own, below the same parent, if there is one to give it.

         Derive (Derivation_Records,
                 Parent (Derivation_Records, Old_Id), New_Id);
         Revoke (Derivation_Records, Old_Id);

         if New_Id /= 0 then
            Layout.Header.Derivation := New_Id;
            Set_Cap (Pid, Cap, Layout);
            Remove_Slot (Derivation_Records, New_Id);
         end if;
      end;
   end Rescind_Cap;

   ------------------
   -- Restart_Call --
   ------------------

   procedure Restart_Call (P : in out Kernel_Process_Entry) is
      use type Rose.Words.Word_32;
   begin
      --  The system call page pushed three words before entering the
      --  kernel, and starts at FFFF_0000.

      P.Stack.EIP := 16#FFFF_0000#;
      P.Stack.ESP := P.Stack.ESP + 12;
      P.Flags (Receive_Any) := False;
      P.Flags (Receive_Cap) := False;
      P.Flags (Invoke_Reply) := False;
      P.Flags (Wait_Reply) := False;
      P.Flags (Interrupt_Resume) := False;
      P.Lent_Buffer_Pages := 0;
   end Restart_Call;

   ------------------
   -- Return_Error --
   ------------------

   procedure Return_Error
     (Params : Rose.Invocation.Invocation_Access;
      Error  : Rose.Invocation.Invocation_Error;
      Data   : Rose.Words.Word := 0)
   is
   begin
      Params.Control.Flags :=
        (Rose.Invocation.Error      => True,
         Rose.Invocation.Reply      => True,
         Rose.Invocation.Send_Words => True,
         others                     => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Data (0) := Data;
      Params.Error := Error;
      --  Params is the kernel's copy of the invocation, which the caller
      --  copies back over Current_Params when the invocation finishes, so
      --  the error has to be in both.
      Current_Process.Current_Params.Data (0) := Data;
      Current_Process.Current_Params.Error := Error;
      Current_Process.Flags (Invoke_Reply) := True;
   end Return_Error;

   --------------------------
   -- Save_Owner_FPU_State --
   --------------------------

   procedure Save_Owner_FPU_State is
   begin
      if FPU_Owner /= null then
         --  TS is set while a process other than the owner runs, and
         --  FXSAVE would fault. The kernel sets it again as it returns
         --  to a process.
         Rose.Arch.Clear_Task_Switched;
         Rose.Arch.Save_FPU_State (FPU_Buffer'Address);
         FPU_Owner.FPU_State := FPU_Buffer;
         FPU_Owner.FPU_Valid := True;
      end if;
   end Save_Owner_FPU_State;

   --------------
   -- Send_Cap --
   --------------

   procedure Send_Cap
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Sender_Cap      : Rose.Capabilities.Capability;
      Receiver_Cap    : Rose.Capabilities.Capability;
      Params          : Rose.Invocation.Invocation_Record)
   is
      pragma Unreferenced (Sender_Cap);
      use Rose.Capabilities;
      use Rose.Invocation;
      From : Kernel_Process_Entry renames Process_Table (From_Process_Id);
      To   : Kernel_Process_Entry renames Process_Table (To_Process_Id);

      Receiver_Expects_Buffer : constant Boolean :=
                                  To.Current_Params.Control.Flags
                                    (Rose.Invocation.Send_Buffer);
      --  A process that receives with Send_Buffer set expects a buffer;
      --  one that does not never sees the sender's (#2).

      function Buffer_Is_Mapped return Boolean;
      --  True if the buffer is in user memory, and each of its pages is a
      --  user page of the sender, writable if the buffer is.
      --  Rose.Kernel.Invocation made sure of this when the sender called,
      --  and Next_Blocked_Sender again if it waited. If it isn't so, the
      --  frames are not the sender's to share, and the buffer is not sent,
      --  rather than give the receiver another process's memory or the
      --  kernel's (#768).

      ----------------------
      -- Buffer_Is_Mapped --
      ----------------------

      function Buffer_Is_Mapped return Boolean is
         use type Rose.Words.Word;
         First  : constant Virtual_Address :=
                    To_Virtual_Address (Params.Buffer_Address);
         Length : constant Virtual_Address :=
                    Virtual_Address (Params.Buffer_Length);
         Write  : constant Boolean :=
                    Params.Control.Flags (Rose.Invocation.Writable_Buffer);
      begin
         if Length = 0 then
            return True;
         end if;

         if First < Virtual_Page_Bytes
           or else First >= Kernel_Virtual_Base
           or else Length > Kernel_Virtual_Base - First
         then
            Debug.Put (From_Process_Id);
            Rose.Boot.Console.Put (": buffer not sent: outside user memory");
            Rose.Boot.Console.New_Line;
            return False;
         end if;

         for Page in Rose.Addresses.Virtual_Address_To_Page (First)
           .. Rose.Addresses.Virtual_Address_To_Page (First + Length - 1)
         loop
            if not Is_User_Page (From_Process_Id, Page)
              or else (Write
                       and then not Is_Writable_User_Page
                         (From_Process_Id, Page))
            then
               Debug.Put (From_Process_Id);
               Rose.Boot.Console.Put (": buffer not sent: page ");
               Rose.Boot.Console.Put (Rose.Words.Word (Page) * 4096);
               Rose.Boot.Console.Put
                 (if Write then " is not writable" else " is not mapped");
               Rose.Boot.Console.New_Line;
               return False;
            end if;
         end loop;

         return True;
      end Buffer_Is_Mapped;
   begin

      To.Current_Params := Params;
      To.Current_Params.Cap := Receiver_Cap;

      if Params.Control.Flags (Rose.Invocation.Send_Caps)
        and then From_Process_Id /= 1
      then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            if Params.Caps (Cap_Index) /= Null_Capability
              and then Has_Cap (From_Process_Id, Params.Caps (Cap_Index))
              and then not Is_Repeated_Single_Use
                (From_Process_Id, Params, Cap_Index)
            then
               declare
                  From_Cap : constant Capability := Params.Caps (Cap_Index);
                  New_Cap  : constant Capability := Create_Cap (To_Process_Id);
               begin
                  if New_Cap = Null_Capability then
                     if Cap_Index > 0 then
                        for Deleted_Cap_Index in 0 .. Cap_Index - 1 loop
                           Delete_Cap
                             (To_Process_Id,
                              To.Current_Params.Caps (Deleted_Cap_Index));
                        end loop;
                     end if;

                     Rose.Boot.Console.Put ("warning: process ");
                     Debug.Put (To_Process_Id);
                     Rose.Boot.Console.Put (": out of capabilities");
                     Rose.Boot.Console.New_Line;

                     Rose.Invocation.Set_Error
                       (Params => From.Current_Params,
                        Error  => Rose.Invocation.Out_Of_Capabilities);
                     return;
                  end if;

                  Set_Cap (To_Process_Id, New_Cap,
                           Cap_Layout (From_Process_Id, From_Cap));
                  To.Current_Params.Caps (Cap_Index) := New_Cap;
               end;
            else

               --  Nothing that the sender holds, so nothing arrives, and
               --  not the number that it named, which is another process's
               --  capability here.

               To.Current_Params.Caps (Cap_Index) := Null_Capability;
            end if;
         end loop;

         Move_Single_Use_Caps (From_Process_Id, Params);
      end if;

      if Params.Control.Flags (Rose.Invocation.Send_Buffer)
        and then (not Receiver_Expects_Buffer
                  or else not Buffer_Is_Mapped)
      then
         To.Current_Params.Control.Flags (Rose.Invocation.Send_Buffer) :=
           False;
         To.Current_Params.Control.Flags (Rose.Invocation.Writable_Buffer) :=
           False;
         To.Current_Params.Buffer_Address := System.Null_Address;
         To.Current_Params.Buffer_Length := 0;

      elsif Params.Control.Flags (Rose.Invocation.Send_Buffer) then
         declare
            use Rose.Words;
            Virtual   : constant Virtual_Address :=
                          To_Virtual_Address (Params.Buffer_Address);
            Page      : Rose.Addresses.Virtual_Page_Address :=
                          Rose.Addresses.Virtual_Address_To_Page
                            (Virtual);
            Base      : constant Rose.Addresses.Virtual_Page_Address :=
                          Page;
            Remaining : Rose.Words.Word :=
                          Rose.Words.Word (Params.Buffer_Length);
            Writable  : constant Boolean :=
                          Params.Control.Flags
                            (Rose.Invocation.Writable_Buffer);
            First     : Rose.Addresses.Virtual_Page_Address := 0;
            Shared    : Rose.Addresses.Virtual_Page_Address;
         begin
            while Remaining > 0 loop
               Share_Page
                 (From_Process_Id, To_Process_Id, Page, Writable, Shared);
               if First = 0 then
                  First := Shared;
               end if;
               Page := Page + 1;
               Remaining := Remaining
                 - Word'Min (Remaining, Virtual_Page_Bytes);
            end loop;

            --  The server writes a writable buffer through its slot, which
            --  mem doesn't see, so a caller's pages are checked again
            --  before it is given the reply (#716).

            if Writable and then From.Flags (Wait_Reply) then
               From.Lent_Buffer_Page := Base;
               From.Lent_Buffer_Pages := Natural (Page - Base);
            end if;

            --  The buffer is where its first page was mapped, which is
            --  not the first slot if the server holds another buffered
            --  request (#529).

            To.Current_Params.Buffer_Address :=
              To_System_Address (Virtual_Page_To_Address (First));
         end;

      end if;

      Set_Current_State (To_Process_Id, Ready);

      if Trace (To_Process_Id) then
         Debug.Put (From_Process_Id);
         Rose.Boot.Console.Put (" -> ");
         Debug.Put (To_Process_Id);
         Rose.Boot.Console.Put (": sending message");
         Rose.Boot.Console.Put (": cap=");
         Rose.Boot.Console.Put (Natural (To.Current_Params.Cap));
         Rose.Boot.Console.New_Line;
         Rose.Invocation.Trace.Put (To.Current_Params, True);
      end if;

   end Send_Cap;

   ---------------
   -- Send_Exit --
   ---------------

   procedure Send_Exit (Pid : Process_Id) is
      use Rose.Invocation;
      Proc   : Kernel_Process_Entry renames Process_Table (Pid);
      Params : Invocation_Record;
   begin
      Proc.Flags (Exit_Pending) := False;

      Params.Control.Flags := (Send => True,
                               others => False);

      Params.Cap := Mem_Page_Fault_Cap;
      Params.Endpoint := Rose.Interfaces.Process.Exit_Process_Endpoint;
      Params.Identifier := Rose.Objects.Process_Identifier (Proc.Oid);

      Send_Word (Params, 0);

      Send_Cap
        (From_Process_Id => Pid,
         To_Process_Id   => Mem_Process,
         Sender_Cap      => 0,
         Receiver_Cap    => Mem_Page_Fault_Cap,
         Params          => Params);
   end Send_Exit;

   -----------------------
   -- Send_Heap_Request --
   -----------------------

   procedure Send_Heap_Request is
      use Rose.Invocation;
      Amount : constant Rose.Addresses.Physical_Bytes := Heap_Request_Amount;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Heap_Request_Pending := False;

      Params.Control.Flags := (Send   => True,
                               Recv_Words => True,
                               others => False);

      Params.Cap := Mem_Page_Fault_Cap;
      Params.Endpoint :=
        Rose.Interfaces.Memory.Take_Physical_Memory_Endpoint;
      Params.Identifier := 0;

      Send_Word (Params, Rose.Words.Word (Amount));
      Params.Control.Last_Recv_Word := 1;

      Params.Reply_Cap :=
        New_Cap
          (Mem_Process,
           Rose.Capabilities.Layout.Capability_Layout'
             (Header  => Rose.Capabilities.Layout.Capability_Header'
                (Cap_Type         => Rose.Capabilities.Layout.Kernel_Cap,
                 Single_Use       => True,
                 Endpoint         => 5,
                 Identifier       => 0,
                 others           => <>),
              Payload => 0));

      Rose.Boot.Console.Put ("kernel: expanding heap by ");
      Rose.Boot.Console.Put (Natural (Amount) / 1024);
      Rose.Boot.Console.Put ("K");
      Rose.Boot.Console.New_Line;

      Send_Cap
        (From_Process_Id => 1,
         To_Process_Id   => Mem_Process,
         Sender_Cap      => 0,
         Receiver_Cap    => Mem_Page_Fault_Cap,
         Params          => Params);
   end Send_Heap_Request;

   ---------------------
   -- Send_Page_Fault --
   ---------------------

   procedure Send_Page_Fault (Pid : Process_Id) is
      use Rose.Invocation;
      Proc   : Kernel_Process_Entry renames Process_Table (Pid);
      Params : Invocation_Record renames Page_Fault_Params;
   begin
      Proc.Flags (Fault_Pending) := False;

      Params.Control.Flags := (Send => True,
                               others => False);

      Params.Cap := Mem_Page_Fault_Cap;
      Params.Endpoint := Rose.Interfaces.Memory.Page_Fault_Endpoint;
      Params.Identifier := 0;

      Send_Object_Id (Params, To_Object_Id (Pid));
      Send_Word (Params, Rose.Words.Word (Proc.Pending_Fault_Page));

      if Proc.Pending_Fault_Mapped then
         Send_Word (Params,
                    Rose.Words.Word
                      (Mapped_Physical_Page
                         (Pid, Proc.Pending_Fault_Page)));
      else
         Send_Word (Params, 0);
      end if;

      if Proc.Pending_Fault_Write then
         Send_Word (Params, 1);
      elsif Proc.Pending_Fault_Execute then
         Send_Word (Params, 2);
      else
         Send_Word (Params, 0);
      end if;

      Send_Cap
        (From_Process_Id => Pid,
         To_Process_Id   => Mem_Process,
         Sender_Cap      => 0,
         Receiver_Cap    => Mem_Page_Fault_Cap,
         Params          => Params);
   end Send_Page_Fault;

   ---------------------------------
   -- Send_Pending_Memory_Request --
   ---------------------------------

   procedure Send_Pending_Memory_Request is
      Pid : Process_Id := Last_Fault_Pid;
   begin
      if Heap_Request_Pending then
         Send_Heap_Request;
         return;
      end if;

      --  Start after the process served last, so that none waits for
      --  ever behind others.

      for Count in Process_Table'Range loop
         Pid := (if Pid >= Process_Table'Last
                 then Process_Table'First
                 else Pid + 1);
         if Process_Table (Pid).Flags (Fault_Pending) then
            Last_Fault_Pid := Pid;
            Send_Page_Fault (Pid);
            return;
         elsif Process_Table (Pid).Flags (Exit_Pending) then
            Last_Fault_Pid := Pid;
            Send_Exit (Pid);
            return;
         end if;
      end loop;
   end Send_Pending_Memory_Request;

   -------------------------
   -- Send_Queued_Message --
   -------------------------

   procedure Send_Queued_Message
     (Process : Process_Id)
   is
      To   : Kernel_Process_Entry renames Process_Table (Process);
   begin
      if To.Flags (Trace) then
         Debug.Put (Process);
         Rose.Boot.Console.Put (": sending queued message");
         Rose.Boot.Console.Put (": cap=");
         Rose.Boot.Console.Put (Natural (To.Queued_Params.Cap));
         Rose.Boot.Console.Put ("; ep=");
         Rose.Boot.Console.Put (Rose.Words.Word_32 (To.Queued_Endpoint));
         Rose.Boot.Console.Put ("; id=");
         Rose.Boot.Console.Put (Natural (To.Queued_Cap_Id));
         Rose.Boot.Console.New_Line;
         Rose.Invocation.Trace.Put (To.Queued_Params, True);
      end if;

      To.Flags (Message_Queued) := False;
      Send_To_Endpoint
        (From_Process_Id => 1,
         To_Process_Id   => Process,
         Sender_Cap      => To.Queued_Params.Cap,
         Endpoint        => To.Queued_Endpoint,
         Identifier      => To.Queued_Cap_Id,
         Params          => To.Queued_Params);

   end Send_Queued_Message;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Params          : Rose.Invocation.Invocation_Record)
   is
      use Rose.Capabilities;
      use Rose.Invocation;
      From : Kernel_Process_Entry renames Process_Table (From_Process_Id);
      To   : Kernel_Process_Entry renames Process_Table (To_Process_Id);
   begin
      if To_Process_Id = 0 then
         return;
      end if;

      if To.State /= Blocked then
         return;
      end if;

      if not To.Flags (Wait_Reply) then
         return;
      end if;

      To.Flags (Wait_Reply) := False;

      To.Current_Params := Params;
      To.Current_Params.Cap := To.Receive_Cap;

      if Params.Control.Flags (Rose.Invocation.Send_Caps) then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            if Params.Caps (Cap_Index) /= Null_Capability
              and then Has_Cap (From_Process_Id, Params.Caps (Cap_Index))
              and then not Is_Repeated_Single_Use
                (From_Process_Id, Params, Cap_Index)
            then
               declare
                  From_Cap : constant Capability := Params.Caps (Cap_Index);
                  New_Cap  : constant Capability := Create_Cap (To_Process_Id);
               begin
                  if New_Cap = Null_Capability then
                     if Cap_Index > 0 then
                        for Deleted_Cap_Index in 0 .. Cap_Index - 1 loop
                           Delete_Cap
                             (To_Process_Id,
                              To.Current_Params.Caps (Deleted_Cap_Index));
                        end loop;
                     end if;

                     Rose.Boot.Console.Put ("warning: process ");
                     Debug.Put (To_Process_Id);
                     Rose.Boot.Console.Put (": out of capabilities");
                     Rose.Boot.Console.New_Line;

                     Rose.Invocation.Set_Error
                       (Params => From.Current_Params,
                        Error  => Rose.Invocation.Out_Of_Capabilities);
                     return;
                  end if;

                  Set_Cap (To_Process_Id, New_Cap,
                           Cap_Layout (From_Process_Id, From_Cap));
                  To.Current_Params.Caps (Cap_Index) := New_Cap;
               end;
            else

               --  Nothing that the sender holds, so nothing arrives, and
               --  not the number that it named, which is another process's
               --  capability here.

               To.Current_Params.Caps (Cap_Index) := Null_Capability;
            end if;
         end loop;

         Move_Single_Use_Caps (From_Process_Id, Params);
      end if;

      Set_Current_State (To_Process_Id, Ready);

   end Send_Reply;

   -------------------------
   -- Send_Restore_Notice --
   -------------------------

   procedure Send_Restore_Notice
     (Process : Process_Id)
   is
      use Rose.Invocation;
      To     : Kernel_Process_Entry renames Process_Table (Process);
      Params : constant Invocation_Record :=
                 Invocation_Record'
                   (Control =>
                      Control_Word'
                        (Flags  => (Send => True, others => False),
                         others => <>),
                    others  => <>);
   begin
      if To.Flags (Trace) then
         Debug.Put (Process);
         Rose.Boot.Console.Put_Line (": sending restore notice");
      end if;

      To.Flags (Restore_Notice) := False;
      Send_To_Endpoint
        (From_Process_Id => 1,
         To_Process_Id   => Process,
         Sender_Cap      => 0,
         Endpoint        =>
           Find_Endpoint
             (Process, Rose.Interfaces.Restorable.Restored_Endpoint),
         Identifier      => 0,
         Params          => Params);
   end Send_Restore_Notice;

   ------------------
   -- Send_Signals --
   ------------------

   procedure Send_Signals
     (Process : Process_Id)
   is
      use Rose.Invocation;
      To     : Kernel_Process_Entry renames Process_Table (Process);
      Params : Invocation_Record :=
                 Invocation_Record'
                   (Control =>
                      Control_Word'
                        (Flags  => (Send       => True,
                                    Send_Words => True,
                                    others     => False),
                         others => <>),
                    others  => <>);
   begin
      if To.Flags (Trace) then
         Debug.Put (Process);
         Rose.Boot.Console.Put_Line (": sending signals");
      end if;

      Params.Data (0) := Rose.Words.Word (To.Signals);
      Params.Control.Last_Sent_Word := 0;
      To.Signals := 0;
      Send_To_Endpoint
        (From_Process_Id => 1,
         To_Process_Id   => Process,
         Sender_Cap      => 0,
         Endpoint        =>
           Find_Endpoint
             (Process, Rose.Interfaces.Signalled.Signal_Endpoint),
         Identifier      => 0,
         Params          => Params);
   end Send_Signals;

   ----------------------
   -- Send_To_Endpoint --
   ----------------------

   procedure Send_To_Endpoint
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Sender_Cap      : Rose.Capabilities.Capability;
      Endpoint        : Rose.Objects.Endpoint_Index;
      Identifier      : Rose.Objects.Capability_Identifier;
      Params          : Rose.Invocation.Invocation_Record)
   is
      use type Rose.Objects.Endpoint_Id;
      use type Rose.Objects.Endpoint_Index;
      To   : Kernel_Process_Entry renames Process_Table (To_Process_Id);
   begin
      if Trace_Launch
        and then Endpoint /= 0
        and then Endpoint = To.Command_Endpoint
      then
         Debug.Put (To_Process_Id);
         Rose.Boot.Console.Put (": trace: launch call received from ");
         Debug.Put (From_Process_Id);
         Rose.Boot.Console.New_Line;
      end if;

      if Endpoint in To.Endpoints'Range
        and then To.Endpoints (Endpoint).Endpoint /= 0
      then
         declare
            Send_Params : constant Rose.Invocation.Invocation_Record :=
                            (Params with delta
                               Endpoint   => To.Endpoints (Endpoint).Endpoint,
                               Identifier => Identifier);

            procedure Queue_Message;
            --  Keep the message until the process next receives.

            -------------------
            -- Queue_Message --
            -------------------

            procedure Queue_Message is
            begin
               if To.Flags (Message_Queued) then
                  Rose.Boot.Console.Put_Line ("lost queued message");
               end if;

               To.Queued_Params := Send_Params;
               To.Queued_Endpoint := Endpoint;
               To.Queued_Cap_Id := Identifier;
               To.Flags (Message_Queued) := True;

               if To.Flags (Trace) then
                  Debug.Put (To_Process_Id);
                  Rose.Boot.Console.Put_Line (": message queued");
                  Rose.Invocation.Trace.Put (To.Queued_Params, True);
               end if;
            end Queue_Message;

         begin
            case Current_State (To_Process_Id) is
               when Blocked =>

                  --  A process that is waiting for the reply to a call
                  --  would get the message as the result of its call, and
                  --  the reply when it arrives as a message: an interrupt
                  --  that arrives while a driver waits for a server does
                  --  this.  So the message waits for its next receive.

                  if To.Flags (Wait_Reply)
                    and then not To.Flags (Receive_Any)
                  then
                     Queue_Message;
                  else
                     Send_Cap (From_Process_Id, To_Process_Id,
                               Sender_Cap, Sender_Cap, Send_Params);
                  end if;
               when Available | Faulted | Killed =>
                  Debug.Put (From_Process_Id);
                  Rose.Boot.Console.Put ("->");
                  Debug.Put (To_Process_Id);
                  Rose.Boot.Console.Put (": to cannot accept messages");
                  Rose.Boot.Console.New_Line;
               when Starting | Ready | Running | Interrupted =>
                  Queue_Message;
            end case;
         end;
         return;
      else
         Rose.Boot.Console.Put ("bad endpoint: ");
         Rose.Boot.Console.Put (Rose.Words.Word_32 (Endpoint));
         Rose.Boot.Console.New_Line;
      end if;
   end Send_To_Endpoint;

   -------------
   -- Set_Cap --
   -------------

   procedure Set_Cap
     (Pid    : Process_Id;
      Cap    : Rose.Capabilities.Capability;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   is
   begin
      if Cap in 1 .. Cap_Tables.Max_Capability_Index then
         Load_Cap_Page (Pid, Cap_Tables.Page_Index (Cap));
      end if;

      declare
         Old : constant Rose.Capabilities.Layout.Capability_Layout :=
                 Cap_Tables.Layout (Process_Table (Pid).Caps, Cap);
         Held : constant Boolean :=
                  Cap_Tables.Is_Active (Process_Table (Pid).Caps, Cap);
      begin
         Cap_Tables.Set (Process_Table (Pid).Caps, Cap, Layout);

         if Held then
            Add_Reference (Layout);
            Drop_Reference (Old);
         end if;
      end;
   end Set_Cap;

   ----------------
   -- Set_Cap_Id --
   ----------------

   procedure Set_Cap_Id
     (Pid    : Process_Id;
      Cap    : Rose.Capabilities.Capability;
      Cap_Id : Rose.Objects.Capability_Identifier)
   is
   begin
      if Cap in 1 .. Cap_Tables.Max_Capability_Index then
         Load_Cap_Page (Pid, Cap_Tables.Page_Index (Cap));
      end if;

      Cap_Tables.Set_Identifier (Process_Table (Pid).Caps, Cap, Cap_Id);
   end Set_Cap_Id;

   ------------------------
   -- Set_Checked_Record --
   ------------------------

   procedure Set_Checked_Record
     (Pid     : Process_Id;
      Address : Rose.Words.Word_32)
   is
   begin
      Checked_Records (Pid) :=
        (Address => Address,
         Changes => Rose.Kernel.Page_Table.Changes,
         Valid   => True);
   end Set_Checked_Record;

   --------------------------
   -- Set_Command_Endpoint --
   --------------------------

   procedure Set_Command_Endpoint
     (Pid      : Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
   is
   begin
      Process_Table (Pid).Command_Endpoint := Endpoint;
   end Set_Command_Endpoint;

   ----------------------------
   -- Set_Current_Invocation --
   ----------------------------

   procedure Set_Current_Invocation
     (Invocation : Rose.Invocation.Invocation_Record)
   is
   begin
      Current_Process.Current_Params := Invocation;
   end Set_Current_Invocation;

   -----------------------
   -- Set_Current_State --
   -----------------------

   procedure Set_Current_State
     (Pid : Process_Id;
      State   : Process_State)
   is
      Current_Process : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      if Current_Process.State = State then
         return;
      end if;

      if Log_State_Changes then
         Debug.Put (Pid);
         Rose.Boot.Console.Put (": state: ");
         Rose.Boot.Console.Put
           (case Current_Process.State is
               when Ready       => "ready",
               when Starting    => "starting",
               when Blocked     => "blocked",
               when Interrupted => "interrupted",
               when Available   => "available",
               when Running     => "running",
               when Faulted     => "faulted",
               when Killed      => "killed");
         Rose.Boot.Console.Put (" -> ");
         Rose.Boot.Console.Put
           (case State is
               when Ready       => "ready",
               when Starting    => "starting",
               when Blocked     => "blocked",
               when Interrupted => "interrupted",
               when Available   => "available",
               when Running     => "running",
               when Faulted     => "faulted",
               when Killed      => "killed");
         Rose.Boot.Console.New_Line;
      end if;

      if Current_Process.State = Killed then
         null;
      elsif Active_Checkpoint
        and then Current_Process.Flags (Persistent)
        and then Current_Process.State = Blocked
        and then State = Ready
      then

         --  The reply would be written into the process's invocation
         --  record, which the checkpoint has already saved as the call,
         --  so a restore would make the call again from the reply (#543).
         --  The process stays blocked until Leave_Checkpoint.

         Current_Process.Flags (Invoke_Reply) := True;
         Current_Process.Flags (Checkpoint_Reply_Held) := True;
      else
         --  A reply held for a page of the invocation record stays
         --  until the process resumes (#715).

         Current_Process.Flags (Invoke_Reply) :=
           (Current_Process.State = Blocked and then State = Ready)
           or else Current_Process.Flags (Reply_Fault);
         Current_Process.Flags (Interrupt_Resume) :=
           Current_Process.State = Interrupted and then State = Ready
           and then not Current_Process.Flags (Call_Restart)
           and then not Current_Process.Flags (Reply_Fault);
         if State = Ready then
            Current_Process.Flags (Call_Restart) := False;
            Current_Process.Flags (Reply_Fault) := False;
         end if;

         Current_Process.State := State;
      end if;

   end Set_Current_State;

   --------------------------
   -- Set_Process_Handlers --
   --------------------------

   procedure Set_Process_Handlers
     (On_Launch     : Rose.Capabilities.Capability;
      On_Page_Fault : Rose.Capabilities.Capability)
   is
   begin
      Mem_Process := Current_Process_Id;
      Mem_Launch_Cap := On_Launch;
      Mem_Page_Fault_Cap := On_Page_Fault;
      Rose.Kernel.Interrupts.Set_Handler
        (Interrupt => Rose.Arch.Interrupts.Page_Fault,
         Handler   => Handle_Page_Fault'Access);

   end Set_Process_Handlers;

   ---------------------
   -- Set_Receive_Cap --
   ---------------------

   procedure Set_Receive_Cap
     (Pid         : Process_Id;
      Endpoint    : Rose.Objects.Endpoint_Index;
      Receive_Cap : Rose.Capabilities.Capability)
   is
      Current_Process : Kernel_Process_Entry renames
                          Process_Table (Pid);
   begin
      --  The kernel has no range checks, and an index outside the array
      --  writes into the fields around it (#861).

      if Endpoint in Registered_Endpoint_Index then
         Current_Process.Endpoints (Endpoint).Receive_Cap := Receive_Cap;
      end if;
   end Set_Receive_Cap;

   ----------------
   -- Share_Page --
   ----------------

   procedure Share_Page
     (From_Process : Rose.Kernel.Processes.Process_Id;
      To_Process   : Rose.Kernel.Processes.Process_Id;
      Address      : Rose.Addresses.Virtual_Page_Address;
      Writable     : Boolean;
      Shared       : out Rose.Addresses.Virtual_Page_Address)
   is
      use Rose.Words;
      Physical_Page      : constant Physical_Page_Address :=
                                Mapped_Physical_Page
                               (Pid          => From_Process,
                                Virtual_Page => Address);
      P                  : constant Kernel_Process_Access :=
                             Process_Table (To_Process)'Access;
      Index              : Shared_Page_Index := 1;
      Virtual_Page       : Virtual_Page_Address :=
                             P.Shared_Pages (Index).Page;
   begin

      while Index < Max_Shared_Pages
        and then P.Shared_Pages (Index).Pid /= 0
      loop
         Index := Index + 1;
         Virtual_Page := Virtual_Page + 1;
      end loop;

      if P.Shared_Pages (Index).Pid /= 0 then
         Rose.Boot.Console.Put ("share page ");
         Debug.Put (From_Process);
         Rose.Boot.Console.Put (" -> ");
         Debug.Put (To_Process);
         Rose.Boot.Console.Put (": out of shared buffers");
         Rose.Boot.Console.New_Line;

         Rose.Boot.Console.Put ("currently sharing with");
         for Rec of P.Shared_Pages loop
            Rose.Boot.Console.Put (" ");
            Debug.Put (Rec.Pid);
         end loop;
         Rose.Boot.Console.New_Line;
         Panic.Panic ("ran out of shared buffers");
      end if;

      P.Shared_Pages (Index) := Shared_Page_Record'
        (Page => Virtual_Page,
         Pid  => From_Process);
      Shared := Virtual_Page;

      Map_Page
        (Pid           => To_Process,
         Virtual_Page  => Virtual_Page,
         Physical_Page => Physical_Page,
         Readable      => True,
         Writable      => Writable,
         Executable    => False,
         User          => True);

      if Log_Shared_Buffers
        or else (Process_Table (From_Process).Flags (Trace)
                 and then Process_Table (To_Process).Flags (Trace))
      then
         Debug.Put (From_Process);
         Rose.Boot.Console.Put (": share page with ");
         Debug.Put (To_Process);
         Rose.Boot.Console.New_Line;

         Rose.Boot.Console.Put (" from ");
         Rose.Boot.Console.Put (Rose.Words.Word (Address) * 4096);
         Rose.Boot.Console.Put (" [");
         Rose.Boot.Console.Put (Rose.Words.Word (Physical_Page) * 4096);
         Rose.Boot.Console.Put ("] to ");
         Rose.Boot.Console.Put (Rose.Words.Word (Virtual_Page) * 4096);
         Rose.Boot.Console.New_Line;
      end if;

   end Share_Page;

   ------------------
   -- Show_Address --
   ------------------

   procedure Show_Address
     (Addr : Rose.Words.Word)
   is
   begin
      Rose.Boot.Console.Put ("Address: ");
      Rose.Boot.Console.Put (Addr);
      Rose.Boot.Console.New_Line;
   end Show_Address;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process
     (Process : Process_Id;
      Address : Rose.Words.Word)
   is
      Proc : constant Kernel_Process_Access :=
               Process_Table (Process)'Access;
   begin
      Proc.Stack :=
        Rose.Kernel.Arch.Process_Stack_Frame
          (Start_EIP => Address);
      Set_Current_State (Process, Ready);
      Rose.Kernel.Processes.Queue.Queue_Process (Process);
   end Start_Process;

   -----------------
   -- Start_Trace --
   -----------------

   procedure Start_Trace (Process : Process_Id) is
   begin
      Process_Table (Process).Flags (Trace) := True;
   end Start_Trace;

   ----------------
   -- Stop_Trace --
   ----------------

   procedure Stop_Trace (Process : Process_Id) is
   begin
      Process_Table (Process).Flags (Trace) := False;
   end Stop_Trace;

   --------------------
   -- Swap_Processes --
   --------------------

   procedure Swap_Processes
     (Old_Pid : Process_Id;
      New_Pid : Process_Id;
      Success : out Boolean)
   is
      use Rose.Capabilities.Layout;
      use type Rose.Capabilities.Capability;
      use type Rose.Objects.Endpoint_Id;
      use type Rose.Objects.Endpoint_Index;
      use type Rose.Objects.Object_Id;
      use type Rose.Objects.Rescinded_Count;
      use type Rose.Words.Word_32;

      Old_Entry : Kernel_Process_Entry renames Process_Table (Old_Pid);
      New_Entry : Kernel_Process_Entry renames Process_Table (New_Pid);
      Old_Oid   : constant Rose.Objects.Object_Id := Old_Entry.Oid;
      New_Oid   : constant Rose.Objects.Object_Id := New_Entry.Oid;

      Map    : array (Registered_Endpoint_Index)
        of Rose.Objects.Endpoint_Index := (others => 0);
      --  Where each of New_Pid's endpoints goes in its new table.

      Source : array (Registered_Endpoint_Index)
        of Rose.Objects.Endpoint_Index := (others => 0);
      --  Which of New_Pid's endpoints each index of the new table holds.

      Table  : Registered_Endpoint_Array;
      --  New_Pid's new endpoint table: Old_Pid's endpoints at their own
      --  indices, then those that only New_Pid has, in the free ones.

      Needed : Natural := 0;
      --  The caps New_Pid needs: one for each reply cap of a call that
      --  waits for Old_Pid, and one for each of Old_Pid's endpoint caps
      --  where New_Pid has none.

      function Old_Has (Index : Rose.Objects.Endpoint_Index) return Boolean
      is (Old_Entry.Endpoints (Index).Endpoint /= 0
          and then Old_Entry.Endpoints (Index).Send_Cap
            /= Rose.Capabilities.Null_Capability);
      --  True if Index is one of Old_Pid's endpoints, with a send cap that
      --  the calls through its clients' caps are checked against.

      procedure Fail (Reason : String);

      ----------
      -- Fail --
      ----------

      procedure Fail (Reason : String) is
      begin
         Debug.Put (Old_Pid);
         Rose.Boot.Console.Put (" -> ");
         Debug.Put (New_Pid);
         Rose.Boot.Console.Put (": cannot swap: ");
         Rose.Boot.Console.Put_Line (Reason);
         Success := False;
      end Fail;

   begin
      Success := False;

      if Old_Pid = New_Pid
        or else not Handovers (Old_Pid).Held
        or else Handovers (Old_Pid).Retired
        or else Handovers (New_Pid).Held
        or else Handovers (New_Pid).Retired
      then
         Fail ("not a handover");
         return;
      end if;

      if Current_Activity (Old_Pid) = Calling
        or else Current_Activity (New_Pid) = Calling
      then

         --  The reply to the call would go to the other process, which
         --  has its object id by then.

         Fail ("a process is in a call");
         return;
      end if;

      if Old_Entry.Flags (Message_Queued) then
         Fail ("a message is queued");
         return;
      end if;

      for I in Registered_Endpoint_Index loop
         if Old_Entry.Endpoints (I).Endpoint /= 0 then
            declare
               J : constant Rose.Objects.Endpoint_Index :=
                     Find_Endpoint (New_Pid, Old_Entry.Endpoints (I).Endpoint);
            begin
               if J = 0 then
                  Fail ("the new process lacks an endpoint");
                  return;
               end if;
               Table (I).Endpoint := Old_Entry.Endpoints (I).Endpoint;
               Map (J) := I;
               Source (I) := J;
            end;
         end if;
      end loop;

      for J in Registered_Endpoint_Index loop
         if New_Entry.Endpoints (J).Endpoint /= 0 and then Map (J) = 0 then
            declare
               K : Rose.Objects.Endpoint_Index := 0;
            begin
               for I in Registered_Endpoint_Index loop
                  if Table (I).Endpoint = 0 then
                     K := I;
                     exit;
                  end if;
               end loop;

               if K = 0 then
                  Fail ("no room for the new process's endpoints");
                  return;
               end if;

               Table (K).Endpoint := New_Entry.Endpoints (J).Endpoint;
               Map (J) := K;
               Source (K) := J;
            end;
         end if;
      end loop;

      declare
         It : Kernel_Process_Access := Old_Entry.Waiting_First;
      begin
         while It /= null loop
            if It.Current_Params.Control.Flags
              (Rose.Invocation.Create_Reply_Cap)
            then
               Needed := Needed + 1;
            end if;
            It := It.Waiting_Next;
         end loop;
      end;

      for I in Registered_Endpoint_Index loop
         if Old_Has (I)
           and then New_Entry.Endpoints (Source (I)).Send_Cap
             = Rose.Capabilities.Null_Capability
         then
            Needed := Needed + 1;
         end if;
      end loop;

      declare
         Spare      : Rose.Capabilities.Capability_Array (1 .. Needed);
         Next_Spare : Positive := 1;

         New_Send   : array (Registered_Endpoint_Index)
           of Capability_Layout;
         --  The layouts of New_Pid's endpoint caps before they change.

         function Take_Spare return Rose.Capabilities.Capability;

         ----------------
         -- Take_Spare --
         ----------------

         function Take_Spare return Rose.Capabilities.Capability is
         begin
            Next_Spare := Next_Spare + 1;
            return Spare (Next_Spare - 1);
         end Take_Spare;

      begin
         if Needed > 0 then
            Create_Caps (New_Pid, Spare);
            if Spare (1) = Rose.Capabilities.Null_Capability then
               Fail ("the new process is out of caps");
               return;
            end if;
         end if;

         --  Nothing fails from here on.

         for J in Registered_Endpoint_Index loop
            New_Send (J) :=
              Cap_Layout (New_Pid, New_Entry.Endpoints (J).Send_Cap);
         end loop;

         --  New_Pid's caps to its own endpoints follow them to their new
         --  indices, and name the object id that it takes. An endpoint
         --  cap to one of Old_Pid's endpoints takes the count of
         --  Old_Pid's, so that its clients' caps stay valid, unless it was
         --  rescinded already.

         for Cap in 1 .. Cap_Tables.Last_Cap (New_Entry.Caps) loop
            if Cap_Tables.Is_Active (New_Entry.Caps, Cap) then
               declare
                  Layout : Capability_Layout := Cap_Layout (New_Pid, Cap);
                  J      : constant Rose.Objects.Endpoint_Index :=
                             Layout.Header.Endpoint;
               begin
                  if Layout.Payload = New_Oid
                    and then (Layout.Header.Cap_Type = Endpoint_Cap
                              or else Layout.Header.Cap_Type = Receive_Cap)
                  then
                     if J in Registered_Endpoint_Index
                       and then Map (J) /= 0
                     then
                        if Layout.Header.Cap_Type = Endpoint_Cap
                          and then Old_Has (Map (J))
                        then
                           declare
                              Old_Send : constant Capability_Layout :=
                                Cap_Layout
                                  (Old_Pid,
                                   Old_Entry.Endpoints (Map (J)).Send_Cap);
                           begin
                              Layout.Header.Rescinded_Count :=
                                (if Cap_Tables.Same_Count
                                   (Layout, New_Send (J))
                                 then Old_Send.Header.Rescinded_Count
                                 else Old_Send.Header.Rescinded_Count + 1);
                           end;
                        end if;
                        Layout.Header.Endpoint := Map (J);
                     end if;
                     Layout.Payload := Old_Oid;
                     Set_Cap (New_Pid, Cap, Layout);
                  end if;
               end;
            end if;
         end loop;

         for I in Registered_Endpoint_Index loop
            if Table (I).Endpoint /= 0 then
               declare
                  J : constant Rose.Objects.Endpoint_Index := Source (I);
               begin
                  Table (I).Receive_Cap := New_Entry.Endpoints (J).Receive_Cap;
                  Table (I).Send_Cap := New_Entry.Endpoints (J).Send_Cap;

                  if Old_Has (I) then
                     if Table (I).Send_Cap = Rose.Capabilities.Null_Capability
                     then
                        Table (I).Send_Cap := Take_Spare;
                     end if;
                     Set_Cap
                       (New_Pid, Table (I).Send_Cap,
                        Cap_Layout
                          (Old_Pid, Old_Entry.Endpoints (I).Send_Cap));
                  end if;
               end;
            end if;
         end loop;

         if New_Entry.Command_Endpoint in Registered_Endpoint_Index then
            New_Entry.Command_Endpoint := Map (New_Entry.Command_Endpoint);
         end if;

         New_Entry.Endpoints := Table;

         --  Calls that wait for New_Pid were made to its launch-time
         --  object id, which Old_Pid takes, and fail as calls to Old_Pid
         --  will. Those that wait for Old_Pid wait for New_Pid, with their
         --  reply caps in its table.

         declare
            It : Kernel_Process_Access := New_Entry.Waiting_First;
         begin
            New_Entry.Waiting_First := null;
            while It /= null loop
               declare
                  Sender : constant Kernel_Process_Access := It;
               begin
                  It := It.Waiting_Next;
                  Sender.Waiting_Next := null;
                  if Sender.Current_Params.Control.Flags
                    (Rose.Invocation.Create_Reply_Cap)
                  then
                     Delete_Cap (New_Pid, Sender.Current_Params.Reply_Cap);
                  end if;
                  Wake_With_Error (Sender.Pid, Rose.Invocation.Closed);
               end;
            end loop;
         end;

         New_Entry.Waiting_First := Old_Entry.Waiting_First;
         Old_Entry.Waiting_First := null;

         declare
            It : Kernel_Process_Access := New_Entry.Waiting_First;
         begin
            while It /= null loop
               if It.Current_Params.Control.Flags
                 (Rose.Invocation.Create_Reply_Cap)
               then
                  declare
                     Reply_Cap : constant Rose.Capabilities.Capability :=
                                   It.Current_Params.Reply_Cap;
                     New_Cap   : constant Rose.Capabilities.Capability :=
                                   Take_Spare;
                  begin
                     Set_Cap (New_Pid, New_Cap,
                              Cap_Layout (Old_Pid, Reply_Cap));
                     Delete_Cap (Old_Pid, Reply_Cap);
                     It.Current_Params.Reply_Cap := New_Cap;
                  end;
               end if;
               It := It.Waiting_Next;
            end loop;
         end;
      end;

      New_Entry.Signals := New_Entry.Signals or Old_Entry.Signals;
      Old_Entry.Signals := 0;

      Old_Entry.Oid := New_Oid;
      New_Entry.Oid := Old_Oid;

      Handovers (New_Pid) :=
        (Held    => True,
         Except  => Handovers (Old_Pid).Except,
         Retired => False);
      Handovers (Old_Pid) :=
        (Held    => False,
         Except  => 0,
         Retired => True);

      Debug.Put (New_Pid);
      Rose.Boot.Console.Put (": takes over from ");
      Debug.Put (Old_Pid);
      Rose.Boot.Console.New_Line;

      Success := True;
   end Swap_Processes;

   ----------------------
   -- Switch_FPU_State --
   ----------------------

   procedure Switch_FPU_State is
   begin
      Rose.Arch.Clear_Task_Switched;

      if FPU_Owner = Current_Process then
         return;
      end if;

      if FPU_Owner /= null then
         Rose.Arch.Save_FPU_State (FPU_Buffer'Address);
         FPU_Owner.FPU_State := FPU_Buffer;
         FPU_Owner.FPU_Valid := True;
      end if;

      if Current_Process.FPU_Valid then
         FPU_Buffer := Current_Process.FPU_State;
         Rose.Arch.Restore_FPU_State (FPU_Buffer'Address);
      else
         Rose.Arch.Initialize_FPU_State;
      end if;

      FPU_Owner := Current_Process;
   end Switch_FPU_State;

   -------------------
   -- To_Process_Id --
   -------------------

   function To_Process_Id
     (Oid : Rose.Objects.Object_Id)
      return Process_Id
   is
      use type Rose.Objects.Object_Id;
   begin
      if not Rose.Objects.Is_Process_Object_Id (Oid) then
         return Null_Process_Id;
      end if;

      for P of Process_Table.all loop
         if P.Oid = Oid and then P.State /= Available then
            return P.Pid;
         end if;
      end loop;

      return Null_Process_Id;
   end To_Process_Id;

   ----------------------
   -- Unblock_And_Send --
   ----------------------

   procedure Unblock_And_Send
     (From_Process_Id : Process_Id;
      To_Process_Id   : Process_Id;
      Receiver_Params : Rose.Invocation.Invocation_Access)
   is
      P : constant Kernel_Process_Access :=
            Process_Table (From_Process_Id)'Access;
   begin
      Send_To_Endpoint
        (From_Process_Id => From_Process_Id,
         To_Process_Id   => To_Process_Id,
         Sender_Cap      => P.Current_Params.Cap,
         Endpoint        => P.Waiting_Endpoint,
         Identifier      => P.Waiting_Cap_Id,
         Params          => P.Current_Params);
      Receiver_Params.all := Process_Table (To_Process_Id).Current_Params;
   end Unblock_And_Send;

   -----------------------------
   -- Unmap_Invocation_Buffer --
   -----------------------------

   procedure Unmap_Invocation_Buffer
     (From_Pid, To_Pid : Process_Id)
   is
      P : Kernel_Process_Entry renames Process_Table (From_Pid);
      --  Base : constant Virtual_Page_Address :=
      --           P.Page_Ranges (Invocation_Range_Index).Base;
      --  Found : Boolean := False;
   begin
      for Rec of P.Shared_Pages loop
         if Rec.Pid = To_Pid then
            if Log_Shared_Buffers
              or else P.Flags (Trace)
            then
               Debug.Put (From_Pid);
               Rose.Boot.Console.Put (" -> ");
               Debug.Put (To_Pid);
               Rose.Boot.Console.Put (": unmap buffer page");
               Rose.Boot.Console.New_Line;
            end if;

            Unmap_Page (From_Pid, Rec.Page);
            Rec.Pid := 0;
         end if;
      end loop;

   end Unmap_Invocation_Buffer;

   ----------------
   -- Unmap_Page --
   ----------------

   procedure Unmap_Page
     (Pid           : Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address)
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      Rose.Kernel.Page_Table.Unmap_Page
        (Directory_Page =>
           Rose.Kernel.Heap.Get_Virtual_Page
             (Physical_Address_To_Page (P.Directory_Page)),
         Virtual_Page   => Virtual_Page);
   end Unmap_Page;

   --------------
   -- Use_Tick --
   --------------

   function Use_Tick return Boolean is
      use Rose.Words;
   begin
      if Current_Process = null then
         return False;
      else
         if Current_Process.Remaining_Ticks > 0 then
            Current_Process.Remaining_Ticks :=
              Current_Process.Remaining_Ticks - 1;
         end if;
         if Current_Process.Remaining_Ticks = 0 then
            Current_Process.Remaining_Ticks :=
              Current_Process.Quantum_Ticks;
            return True;
         else
            return False;
         end if;
      end if;
   end Use_Tick;

   -----------------------
   -- Wait_For_Receiver --
   -----------------------

   procedure Wait_For_Receiver
     (Waiting_Process_Id   : Process_Id;
      Receiving_Process_Id : Process_Id;
      Endpoint             : Rose.Objects.Endpoint_Index;
      Identifier           : Rose.Objects.Capability_Identifier;
      Params               : Rose.Invocation.Invocation_Record)
   is
      P : constant Kernel_Process_Access :=
            Process_Table (Waiting_Process_Id)'Access;
      Q : constant Kernel_Process_Access :=
            Process_Table (Receiving_Process_Id)'Access;
   begin
      Set_Current_State (Waiting_Process_Id, Blocked);
      P.Current_Params := Params;
      P.Waiting_Endpoint := Endpoint;
      P.Waiting_Cap_Id := Identifier;
      P.Waiting_Next := null;
      if Q.Waiting_First = null then
         Q.Waiting_First := P;
      else
         declare
            R : Kernel_Process_Access := Q.Waiting_First;
         begin
            while R.Waiting_Next /= null loop
               R := R.Waiting_Next;
            end loop;
            R.Waiting_Next := P;
         end;
      end if;
   end Wait_For_Receiver;

   --------------------
   -- Wait_For_Reply --
   --------------------

   procedure Wait_For_Reply (Pid : Process_Id) is
   begin
      Process_Table (Pid).Flags (Wait_Reply) := True;
   end Wait_For_Reply;

   ----------------------------
   -- Wake_Waiting_Processes --
   ----------------------------

   procedure Wake_Waiting_Processes (Pid : Process_Id) is
      use type Rose.Capabilities.Layout.Capability_Type;
      P    : Kernel_Process_Entry renames Process_Table (Pid);
      Next : Kernel_Process_Access := P.Waiting_First;
   begin
      P.Waiting_First := null;
      while Next /= null loop
         declare
            Sender : constant Kernel_Process_Access := Next;
         begin
            Next := Sender.Waiting_Next;
            Sender.Waiting_Next := null;
            Wake_With_Error (Sender.Pid, Rose.Invocation.Closed);
         end;
      end loop;

      --  A caller waiting for the reply is named by the payload of the
      --  reply cap that the dead process holds for it.

      for Cap in 1 .. Cap_Tables.Last_Cap (P.Caps) loop
         if Cap_Tables.Is_Active (P.Caps, Cap) then
            declare
               Layout : constant Rose.Capabilities.Layout.Capability_Layout :=
                 Cap_Layout (Pid, Cap);
            begin
               if Layout.Header.Cap_Type = Rose.Capabilities.Layout.Reply_Cap
               then
                  declare
                     Caller : constant Process_Id :=
                       To_Process_Id (Layout.Payload);
                  begin
                     if Caller /= Null_Process_Id
                       and then Caller /= Pid
                       and then Process_Table (Caller).State = Blocked
                       and then Process_Table (Caller).Flags (Wait_Reply)
                     then
                        Wake_With_Error (Caller, Rose.Invocation.Closed);
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;
   end Wake_Waiting_Processes;

   ---------------------
   -- Wake_With_Error --
   ---------------------

   procedure Wake_With_Error
     (Pid   : Process_Id;
      Error : Rose.Invocation.Invocation_Error)
   is
      W : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      if Trace_Launch or else W.Flags (Trace) then
         Debug.Put (Pid);
         Rose.Boot.Console.Put_Line
           (": woken: the process it waited on has gone");
      end if;

      W.Flags (Wait_Reply) := False;
      W.Current_Params.Control.Flags :=
        (Rose.Invocation.Error => True,
         Rose.Invocation.Reply => True,
         others                => False);
      W.Current_Params.Control.Last_Sent_Word := 0;
      W.Current_Params.Error := Error;
      W.Current_Params.Cap := W.Receive_Cap;
      Set_Current_State (Pid, Ready);
   end Wake_With_Error;

end Rose.Kernel.Processes;
