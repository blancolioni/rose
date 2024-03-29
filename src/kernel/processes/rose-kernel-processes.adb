with Rose.Boot.Console;

with Rose.Arch.Interrupts;
with Rose.Kernel.Page_Table;
with Rose.Kernel.Processes.Debug;
with Rose.Kernel.Processes.Queue;
with Rose.Kernel.Processes;

with Rose.Kernel.Heap;

with Rose.Kernel.Panic;
with Rose.Invocation.Trace;

package body Rose.Kernel.Processes is

   Log_Shared_Buffers : constant Boolean := False;
   Log_State_Changes  : constant Boolean := False;
   Log_Cap_Cache      : constant Boolean := False;

   First_Persistent_Pid : constant Process_Id := 0;

   Active_Checkpoint : Boolean := False;

   procedure Show_Address
     (Addr : Rose.Words.Word);
   pragma Export (C, Show_Address, "debug_show_address");

   Current_Page_Fault_Count  : Natural := 0;
   Page_Fault_Params         : aliased Rose.Invocation.Invocation_Record;

   Page_Fault_Address : Rose.Words.Word_32;
   pragma Import (C, Page_Fault_Address, "page_fault_address");

   function Handle_Page_Fault
     (Code : Rose.Words.Word)
      return Rose.Kernel.Interrupts.Interrupt_Handler_Status;

   procedure Share_Page
     (From_Process : Rose.Kernel.Processes.Process_Id;
      To_Process   : Rose.Kernel.Processes.Process_Id;
      Address      : Rose.Addresses.Virtual_Page_Address;
      Writable     : Boolean);

   procedure Create_Process_Table_Entry
     (Pid : Rose.Kernel.Processes.Process_Id;
      Name : String);

   procedure Disable_Write_Pages
     (Process : in out Kernel_Process_Entry);

   procedure Enable_Write_Pages
     (Process : in out Kernel_Process_Entry);

   ----------------
   -- Cap_Layout --
   ----------------

   function Cap_Layout
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Layout
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      return P.Cached_Caps (Load_Cap (Pid, Cap)).Layout;
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
      Copy_Cap_Layout (From_Process_Id, Cap,
                       To_Process_Id, To_Cap);
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
      use Rose.Capabilities;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      for Cap in Capability range 1 .. Max_Capability_Index loop
         if not P.Is_Active (Cap) then
            P.Is_Active (Cap) := True;
            if Cap > P.Last_Cap then
               P.Last_Cap := Cap;
            end if;
            return Cap;
         end if;
      end loop;
      Debug.Put (Pid);
      Rose.Boot.Console.Put_Line (": out of capabilities");
      return 0;
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

      Proc.Oid := Rose.Objects.Object_Id (Pid);
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

      Proc.Page_Flags := (others => (others => False));
      Proc.Page_Ranges := (others => (others => 0));

      Directory_VP := Rose.Kernel.Heap.Allocate_Page;
      Physical_Directory_Page :=
        Rose.Kernel.Heap.Get_Physical_Page (Directory_VP);

      Proc.Directory_Page :=
        Physical_Page_To_Address (Physical_Directory_Page);

      Rose.Kernel.Page_Table.Init_User_Page_Directory (Directory_VP);
      Proc.Env_Page := Rose.Kernel.Heap.Allocate_Page;

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
           Virtual_Address_To_Page (Environment_Base),
         Physical_Page  => Proc.Env_Page,
         Readable       => True,
         Writable       => True,
         Executable     => False,
         User           => True);

      Map_Page
        (Directory_Page => Directory_VP,
         Virtual_Page   =>
           Virtual_Address_To_Page (Invocation_Buffer_Range_Base),
         Physical_Page  => Proc.Env_Page,
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

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
   is
   begin
      Set_Cap (Pid, Cap, (others => <>));
      Process_Table (Pid).Is_Active (Cap) := False;
   end Delete_Cap;

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

   -----------------
   -- Expand_Heap --
   -----------------

   procedure Expand_Heap
     (Amount : Rose.Addresses.Physical_Bytes)
   is
      use Rose.Invocation;
      P : Kernel_Process_Entry renames Process_Table (Mem_Process);
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if P.State = Blocked then
         Params.Control.Flags := (Send   => True,
                                  Recv_Words => True,
                                  others => False);

         Params.Cap := Mem_Page_Fault_Cap;
         Params.Endpoint := 16#2363_36CC_C1A7#;
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

      end if;
   end Expand_Heap;

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
      P : Kernel_Process_Entry renames Process_Table (Pid);
      Index : constant Cached_Capability_Index := Load_Cap (Pid, Cap);
      Full_Cap : Rose.Capabilities.Layout.Capability_Layout
      renames P.Cached_Caps (Index).Layout;
   begin
      Layout := Full_Cap;
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
      use Rose.Invocation;
      Params : Invocation_Record renames Page_Fault_Params;
   begin

      if Active_Checkpoint
        and then Is_Mapped
        and then Write_Attempt
      then
         --  write attempt failed because of active checkpoint.
         --  record situation and return

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
         loop
            null;
         end loop;
      end if;

      Params.Control.Flags := (Send => True,
                               others => False);

      Params.Cap := Mem_Page_Fault_Cap;
      Params.Endpoint := 16#C6FF_984F_29F6#;
      Params.Identifier := 0;

      Send_Object_Id (Params, To_Object_Id (Pid));
      Send_Word (Params, Rose.Words.Word (Virtual_Page));

      if Is_Mapped then
         Send_Word (Params,
                    Rose.Words.Word
                      (Mapped_Physical_Page
                         (Pid, Virtual_Page)));
      else
         Send_Word (Params, 0);
      end if;

      if Write_Attempt then
         Send_Word (Params, 1);
      elsif Execution_Attempt then
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

   end Handle_Page_Fault;

   -------------
   -- Has_Cap --
   -------------

   function Has_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Capabilities.Capability;
      use Rose.Capabilities.Layout;
      Layout : Capability_Layout;
   begin
      if Cap > Process_Table (Pid).Last_Cap
        or else not Process_Table (Pid).Is_Active (Cap)
      then
         return False;
      else
         Get_Cap (Pid, Cap, Layout);
         return Layout.Header.Cap_Type /= Null_Cap;
      end if;
   end Has_Cap;

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

   ---------------------------
   -- Have_Process_Handlers --
   ---------------------------

   function Have_Process_Handlers return Boolean is
   begin
      return Mem_Process /= 0;
   end Have_Process_Handlers;

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
            use type Rose.Objects.Rescinded_Count;
            Send_Cap : constant Rose.Capabilities.Capability :=
                         To.Endpoints (Endpoint).Send_Cap;
            Rescinded_Count : constant Rose.Objects.Rescinded_Count :=
                                Cap_Layout (Destination_Process, Send_Cap)
                                .Header.Rescinded_Count;
         begin
            return Source_Cap.Header.Rescinded_Count = Rescinded_Count;
         end;
      end if;
      return False;
   end Is_Valid_Entry;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process
     (Process : Process_Id)
   is
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      P.State := Available;
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
         end if;
      end loop;
   end Leave_Checkpoint;

   --------------
   -- Load_Cap --
   --------------

   function Load_Cap
     (Pid : Process_Id;
      Cap : Rose.Capabilities.Capability)
      return Cached_Capability_Index
   is
      use type Rose.Capabilities.Capability;
      use type Rose.Words.Word_32;
      P : Kernel_Process_Entry renames Process_Table (Pid);
      Result : Cached_Capability_Count := 0;
   begin
      if P.Is_Cached (Cap) then
         for I in P.Cached_Caps'Range loop
            if P.Cached_Caps (I).Cap = Cap then
               Result := I;
               exit;
            end if;
         end loop;
      end if;

      if Result = 0 then
         declare
            use Rose.Words;
            Min_Tick : Word_32 := Word_32'Last;
            Save_Cap : Boolean := True;
         begin
            for I in P.Cached_Caps'Range loop
               declare
                  Cached : Cached_Capability renames P.Cached_Caps (I);
               begin
                  if Cached.Cap = 0 then
                     Result := I;
                     Save_Cap := False;
                     exit;
                  end if;
                  if Cached.Tick < Min_Tick then
                     Min_Tick := Cached.Tick;
                     Result := I;
                  end if;
               end;
            end loop;

            if Save_Cap then
               declare
                  Cache : Cached_Capability renames
                            P.Cached_Caps (Result);
                  Page_Index : constant Capability_Page_Index :=
                                 Get_Cap_Page_Index (Cache.Cap);
               begin
                  if Log_Cap_Cache and then P.Flags (Trace) then
                     Debug.Put (Pid);
                     Rose.Boot.Console.Put (": dropping cap: ");
                     Rose.Boot.Console.Put (Natural (Cache.Cap));
                     Rose.Boot.Console.Put (" age=");
                     Rose.Boot.Console.Put (Natural (Min_Tick));
                     Rose.Boot.Console.New_Line;
                  end if;

                  Load_Cap_Page (Pid, Page_Index);

                  declare
                     Page : Capability_Page;
                     pragma Import (Ada, Page);
                     for Page'Address use P.Cap_Pages (Page_Index);
                  begin
                     Page (Cache.Cap mod Capabilities_Per_Page + 1) :=
                       Cache.Layout;
                  end;
               end;
            end if;
         end;

         declare
            Cached : Cached_Capability renames P.Cached_Caps (Result);
            Page_Index : constant Capability_Page_Index :=
                           Get_Cap_Page_Index (Cap);
         begin
            if Cached.Cap /= 0 then
               P.Is_Cached (Cached.Cap) := False;
            end if;

            if Log_Cap_Cache and then P.Flags (Trace) then
               Debug.Put (Pid);
               Rose.Boot.Console.Put (": loading cap: ");
               Rose.Boot.Console.Put (Natural (Cap));
               Rose.Boot.Console.Put (" age=");
               Rose.Boot.Console.Put (Natural (P.Next_Cap_Tick) + 1);
               Rose.Boot.Console.New_Line;
            end if;

            Load_Cap_Page (Pid, Page_Index);
            declare
               Page : Capability_Page;
               pragma Import (Ada, Page);
               for Page'Address use P.Cap_Pages (Page_Index);
            begin
               P.Cached_Caps (Result) :=
                 Cached_Capability'
                   (Cap    => Cap,
                    Tick   => 0,
                    Layout => Page (Cap mod Capabilities_Per_Page + 1));
            end;
         end;
      end if;

      P.Is_Cached (Cap) := True;
      P.Next_Cap_Tick := P.Next_Cap_Tick + 1;
      P.Cached_Caps (Result).Tick := P.Next_Cap_Tick;
      return Result;
   end Load_Cap;

   -------------------
   -- Load_Cap_Page --
   -------------------

   procedure Load_Cap_Page
     (Pid  : Process_Id;
      Page : Capability_Page_Index)
   is
      use System;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      if P.Cap_Pages (Page) = Null_Address then
         declare
            Page_Address : constant Virtual_Page_Address :=
                             Rose.Kernel.Heap.Allocate_Page;
         begin
            P.Cap_Pages (Page) :=
              System'To_Address (Virtual_Page_To_Address (Page_Address));
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
      User          : Boolean)
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
         User           => User);
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
      Pid               : Process_Id := Next_Pid;
   begin

      while Process_Table (Pid).State /= Available loop
         Pid := Pid + 1;
      end loop;

      Create_Process_Table_Entry (Pid, Name);

      Next_Pid := Pid + 1;
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
      It   : Kernel_Process_Access :=
               Process_Table (Receiver_Id).Waiting_First;
      Prev : Kernel_Process_Access := null;
   begin
      if Endpoint /= 0 then
         while It /= null
           and then It.Waiting_Endpoint /= Endpoint
         loop
            Prev := It;
            It := It.Waiting_Next;
         end loop;
      end if;

      if It /= null then
         if Prev = null then
            Process_Table (Receiver_Id).Waiting_First := It.Waiting_Next;
         else
            Prev.Waiting_Next := It.Waiting_Next;
         end if;

         It.Waiting_Next := null;
         return It.Pid;
      end if;
      return 0;
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
      procedure Update
        (Layout : in out Rose.Capabilities.Layout.Capability_Layout);

      ------------
      -- Update --
      ------------

      procedure Update
        (Layout : in out Rose.Capabilities.Layout.Capability_Layout)
      is
         use type Rose.Objects.Rescinded_Count;
      begin
         Layout.Header.Rescinded_Count :=
           Layout.Header.Rescinded_Count + 1;
      end Update;

   begin
      Update_Cap (Pid, Cap, Update'Access);
   end Rescind_Cap;

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
      Current_Process.Current_Params.Data (0) := Data;
      Current_Process.Current_Params.Error := Error;
      Current_Process.Flags (Invoke_Reply) := True;
   end Return_Error;

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
   begin

      To.Current_Params := Params;
      To.Current_Params.Cap := Receiver_Cap;

      if Params.Control.Flags (Rose.Invocation.Send_Caps)
        and then From_Process_Id /= 1
      then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            if Params.Caps (Cap_Index) /= Null_Capability then
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
            end if;
         end loop;
      end if;

      if Params.Control.Flags (Rose.Invocation.Send_Buffer) then
         declare
            use Rose.Words;
            Virtual   : constant Virtual_Address :=
                          To_Virtual_Address (Params.Buffer_Address);
            Page      : Rose.Addresses.Virtual_Page_Address :=
                          Rose.Addresses.Virtual_Address_To_Page
                            (Virtual);
            Remaining : Rose.Words.Word :=
                          Rose.Words.Word (Params.Buffer_Length);
            Writable  : constant Boolean :=
                          Params.Control.Flags
                            (Rose.Invocation.Writable_Buffer);
         begin
            while Remaining > 0 loop
               Share_Page (From_Process_Id, To_Process_Id, Page, Writable);
               Page := Page + 1;
               Remaining := Remaining
                 - Word'Min (Remaining, Virtual_Page_Bytes);
            end loop;
         end;

         To.Current_Params.Buffer_Address :=
           To_System_Address
             (Virtual_Page_To_Address
              (To.Page_Ranges (Invocation_Range_Index).Base));

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
            if Params.Caps (Cap_Index) /= Null_Capability then
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
            end if;
         end loop;
      end if;

      Set_Current_State (To_Process_Id, Ready);

   end Send_Reply;

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
      To   : Kernel_Process_Entry renames Process_Table (To_Process_Id);
   begin
      if Endpoint in To.Endpoints'Range
        and then To.Endpoints (Endpoint).Endpoint /= 0
      then
         declare
            Send_Params : constant Rose.Invocation.Invocation_Record :=
                            (Params with delta
                               Endpoint   => To.Endpoints (Endpoint).Endpoint,
                               Identifier => Identifier);
         begin
            case Current_State (To_Process_Id) is
               when Blocked =>
                  Send_Cap (From_Process_Id, To_Process_Id,
                            Sender_Cap, Sender_Cap, Send_Params);
               when Available | Faulted | Killed =>
                  Debug.Put (From_Process_Id);
                  Rose.Boot.Console.Put ("->");
                  Debug.Put (To_Process_Id);
                  Rose.Boot.Console.Put (": to cannot accept messages");
                  Rose.Boot.Console.New_Line;
               when Starting | Ready | Running | Interrupted =>
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
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      P.Cached_Caps (Load_Cap (Pid, Cap)).Layout := Layout;
   end Set_Cap;

   ----------------
   -- Set_Cap_Id --
   ----------------

   procedure Set_Cap_Id
     (Pid    : Process_Id;
      Cap    : Rose.Capabilities.Capability;
      Cap_Id : Rose.Objects.Capability_Identifier)
   is
      procedure Update
        (Layout : in out Rose.Capabilities.Layout.Capability_Layout);

      ------------
      -- Update --
      ------------

      procedure Update
        (Layout : in out Rose.Capabilities.Layout.Capability_Layout)
      is
      begin
         Layout.Header.Identifier := Cap_Id;
      end Update;

   begin
      Update_Cap (Pid, Cap, Update'Access);
   end Set_Cap_Id;

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
      else
         Current_Process.Flags (Invoke_Reply) :=
           Current_Process.State = Blocked and then State = Ready;
         Current_Process.Flags (Interrupt_Resume) :=
           Current_Process.State = Interrupted and then State = Ready;

         Current_Process.State := State;
      end if;

   end Set_Current_State;

   --------------------------
   -- Set_Process_Handlers --
   --------------------------

   procedure Set_Process_Handlers
     (On_Launch     : Rose.Capabilities.Capability;
      On_Kill       : Rose.Capabilities.Capability;
      On_Page_Fault : Rose.Capabilities.Capability)
   is
   begin
      Mem_Process := Current_Process_Id;
      Mem_Launch_Cap := On_Launch;
      Mem_Kill_Cap   := On_Kill;
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
      Rec             : Registered_Endpoint_Record renames
                          Current_Process.Endpoints (Endpoint);
   begin
      Rec.Receive_Cap := Receive_Cap;
   end Set_Receive_Cap;

   ----------------
   -- Share_Page --
   ----------------

   procedure Share_Page
     (From_Process : Rose.Kernel.Processes.Process_Id;
      To_Process   : Rose.Kernel.Processes.Process_Id;
      Address      : Rose.Addresses.Virtual_Page_Address;
      Writable     : Boolean)
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

   -------------------
   -- To_Process_Id --
   -------------------

   function To_Process_Id
     (Oid : Rose.Objects.Object_Id)
      return Process_Id
   is
      use Rose.Objects;
   begin
      if Oid in Persistent_Process_Object_Id then
         return Process_Id (Oid - Persistent_Process_Object_Id'First)
           + First_Persistent_Pid;
      else
         return Process_Id (Oid);
      end if;
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

   ----------------
   -- Update_Cap --
   ----------------

   procedure Update_Cap
     (Pid    : Process_Id;
      Cap    : Rose.Capabilities.Capability;
      Update : not null access
        procedure
          (Layout : in out Rose.Capabilities.Layout.Capability_Layout))
   is
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      Update (P.Cached_Caps (Load_Cap (Pid, Cap)).Layout);
   end Update_Cap;

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

end Rose.Kernel.Processes;
