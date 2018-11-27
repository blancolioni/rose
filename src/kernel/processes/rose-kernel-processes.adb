with Rose.Boot.Console;

with Rose.Arch.Interrupts;
with Rose.Kernel.Page_Table;
with Rose.Kernel.Processes.Debug;
with Rose.Kernel.Processes;

with Rose.Kernel.Panic;
with Rose.Kernel.Validation;

with Rose.Invocation.Trace;

package body Rose.Kernel.Processes is

   Log_Shared_Buffers : constant Boolean := False;
   Log_State_Changes  : constant Boolean := False;
   Log_Page_Faults    : constant Boolean := False;

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
     (From_Process : Rose.Objects.Process_Id;
      To_Process   : Rose.Objects.Process_Id;
      Address      : Rose.Addresses.Virtual_Page_Address;
      Writable     : Boolean);

   --------------
   -- Cap_Type --
   --------------

   function Cap_Type
     (Process_Id : Rose.Objects.Process_Id;
      Cap        : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Type
   is
   begin
      if Is_Valid_Process_Id (Process_Id)
        and then Has_Cap (Process_Id, Cap)
      then
         return Process_Table (Process_Id).Cap_Cache (Cap).Header.Cap_Type;
      else
         return Rose.Capabilities.Layout.Null_Cap;
      end if;
   end Cap_Type;

   ---------------------
   -- Copy_Cap_Layout --
   ---------------------

   procedure Copy_Cap_Layout
     (From_Process : Rose.Objects.Process_Id;
      From_Cap     : Rose.Capabilities.Capability;
      To_Process   : Rose.Objects.Process_Id;
      To_Cap       : Rose.Capabilities.Capability)
   is
   begin
      Process_Table (To_Process).Cap_Cache (To_Cap) :=
        Process_Table (From_Process).Cap_Cache (From_Cap);
   end Copy_Cap_Layout;

   ----------------
   -- Create_Cap --
   ----------------

   function Create_Cap
     (Process_Id : Rose.Objects.Process_Id)
      return Rose.Capabilities.Capability
   is
      use Rose.Capabilities, Rose.Capabilities.Layout;
      P : Kernel_Process_Entry renames Process_Table (Process_Id);
   begin
      for I in P.Cap_Cache'Range loop
         if P.Cap_Cache (I).Header.Cap_Type = Null_Cap then
            return I;
         end if;
      end loop;

      --  FIXME: implement cap pages
      return Null_Capability;
   end Create_Cap;

   -----------------
   -- Create_Caps --
   -----------------

   function Create_Caps
     (Process_Id : Rose.Objects.Process_Id;
      Count      : Natural)
      return Rose.Capabilities.Capability
   is
      use Rose.Capabilities, Rose.Capabilities.Layout;
      P           : Kernel_Process_Entry renames Process_Table (Process_Id);
      Found_Count : Natural := 0;
      Start_Cap   : Capability := Null_Capability;
   begin
      for I in P.Cap_Cache'Range loop
         if P.Cap_Cache (I).Header.Cap_Type = Null_Cap then
            if Found_Count = 0 then
               Start_Cap := I;
            end if;

            Found_Count := Found_Count + 1;
            if Found_Count = Count then
               return Start_Cap;
            end if;
         else
            Found_Count := 0;
         end if;
      end loop;

      --  FIXME: implement cap pages
      return Null_Capability;
   end Create_Caps;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint
     (Process_Id : Rose.Objects.Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Objects.Endpoint_Index
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Process_Id);
   begin
      for I in P.Endpoints'Range loop
         if P.Endpoints (I).Endpoint = Rose.Objects.Null_Endpoint_Id then
            P.Endpoints (I).Endpoint := Endpoint;
            return I;
         end if;
      end loop;
      return 0;
   end Create_Endpoint;

   -----------------
   -- Current_EIP --
   -----------------

   function Current_EIP
     (Process : Rose.Objects.Process_Id)
      return Rose.Words.Word_32
   is
   begin
      return Process_Table (Process).Stack.EIP;
   end Current_EIP;

   -----------------
   -- Current_ESP --
   -----------------

   function Current_ESP
     (Process : Rose.Objects.Process_Id)
      return Rose.Words.Word_32
   is
   begin
      return Process_Table (Process).Stack.ESP;
   end Current_ESP;

   -------------------------
   -- Current_Process_Cap --
   -------------------------

   function Current_Process_Cap
     (Cap_Index : Rose.Capabilities.Capability;
      Cap       : out Rose.Capabilities.Layout.Capability_Layout)
      return Boolean
   is
      use Rose.Capabilities;
      use all type Rose.Capabilities.Layout.Capability_Type;
   begin
      if Cap_Index = 0 then
         return False;
      elsif Cap_Index <= Cached_Capability_Count then
         Cap := Current_Process.Cap_Cache (Cap_Index);
      elsif Cap_Index <= Max_Capability_Index then
         declare
            Page_Index : constant Positive :=
                           Natural (Cap_Index - Cached_Capability_Count)
                           / Capabilities_Per_Page + 1;
         begin
            if Current_Process.Cap_Pages (Page_Index) = 0 then
               return False;
            end if;
            --  FIXME: implement
            return False;
         end;
      else
         return False;
      end if;

      if Cap.Header.Cap_Type = Null_Cap then
         return False;
      end if;

      declare
         use type Rose.Objects.Allocation_Count;
      begin
         if Cap.Header.Alloc_Count > 0 then
            Cap.Header.Alloc_Count := Cap.Header.Alloc_Count - 1;
            if Cap.Header.Alloc_Count = 0 then
               Current_Process.Cap_Cache (Cap_Index) :=
                 Rose.Capabilities.Layout.Empty_Capability;
               Rose.Kernel.Validation.Delete_Cap
                 (Current_Process_Id, Cap_Index);
            else
               Current_Process.Cap_Cache (Cap_Index) := Cap;
            end if;
         end if;
      end;

      return True;
   end Current_Process_Cap;

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap
     (Process_Id : Rose.Objects.Process_Id;
      Cap        : Rose.Capabilities.Capability)
   is
      P : Kernel_Process_Entry renames Process_Table (Process_Id);
   begin
      P.Cap_Cache (Cap) := (others => <>);
   end Delete_Cap;

   -----------------------
   -- Find_Endpoint_Cap --
   -----------------------

   function Find_Endpoint_Cap
     (Process_Id : Rose.Objects.Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Process_Id);
   begin
      for Rec of P.Endpoints loop
         if Rec.Endpoint = Endpoint then
            return Rec.Send_Cap;
         end if;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Find_Endpoint_Cap;

   ------------------------
   -- Get_Page_Object_Id --
   ------------------------

   function Get_Page_Object_Id
     (Process : Rose.Objects.Process_Id;
      Address : Rose.Words.Word)
      return Rose.Objects.Page_Object_Id
   is
      use Rose.Objects;
      use Rose.Words;
      Result : Page_Object_Id := Page_Object_Id'First;
   begin
      Result := Result +
        Page_Object_Id (Object_Id (Process)) * 2 ** 32;
      Result := Result +
        Page_Object_Id
          (Object_Id (Address / Rose.Addresses.Physical_Page_Bytes));
      return Result;
   end Get_Page_Object_Id;

   --------------------------
   -- Get_Page_Object_Page --
   --------------------------

   function Get_Page_Object_Page
     (Page_Object : Rose.Objects.Page_Object_Id)
      return Rose.Addresses.Virtual_Page_Address
   is
      use Rose.Words;
   begin
      return Rose.Addresses.Virtual_Page_Address
        (Word_64 (Page_Object) mod 2 ** 32);
   end Get_Page_Object_Page;

   -----------------------------
   -- Get_Page_Object_Process --
   -----------------------------

   function Get_Page_Object_Process
     (Page_Object : Rose.Objects.Page_Object_Id)
      return Rose.Objects.Process_Id
   is
      use Rose.Words;
   begin
      return Rose.Objects.Process_Id
        (Word_64 (Page_Object) / 2 ** 32 mod 2 ** 24);
   end Get_Page_Object_Process;

   -------------------------------------
   -- Handle_General_Protection_Fault --
   -------------------------------------

   function Handle_General_Protection_Fault
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status
   is
   begin
      Rose.Boot.Console.Put (Current_Process_Id);
      Rose.Boot.Console.Put_Line (": general protection fault");
      Debug.Report_Process (Current_Process_Id, True);
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
      use type Rose.Objects.Process_Id;
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

      if Log_Page_Faults or else Current_Process_Id = Log_Process_Activity then
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
         Debug.Report_Process (Current_Process_Id, False);
      end if;

      Current_Page_Fault_Count := Current_Page_Fault_Count + 1;

      Handle_Page_Fault
        (Virtual_Page      => Virtual_Page,
         Is_Mapped         => not Protection_Violation,
         Read_Attempt      => not Write_Attempt and then not Execution_Attempt,
         Write_Attempt     => Write_Attempt,
         Execution_Attempt => Execution_Attempt);

      return Rose.Kernel.Interrupts.Not_Finished;
   end Handle_Page_Fault;

   -----------------------
   -- Handle_Page_Fault --
   -----------------------

   procedure Handle_Page_Fault
     (Virtual_Page      : Rose.Addresses.Virtual_Page_Address;
      Is_Mapped         : Boolean;
      Read_Attempt      : Boolean;
      Write_Attempt     : Boolean;
      Execution_Attempt : Boolean)
   is
      use type Rose.Objects.Process_Id;
      pragma Unreferenced (Read_Attempt);
      use Rose.Invocation;
      Params : Invocation_Record renames Page_Fault_Params;
   begin

      if Current_Process_Id = Mem_Process then
         Rose.Kernel.Processes.Debug.Report_Process
           (Rose.Kernel.Processes.Current_Process_Id, True);
         Rose.Kernel.Panic.Panic
           ("page fault in page fault handler process");
         loop
            null;
         end loop;
      end if;

      Params.Control.Flags := (Send => True, Send_Words => True,
                               others => False);

      Params.Control.Last_Sent_Word := 3;
      Params.Cap := Mem_Page_Fault_Cap;
      Params.Endpoint := 102;
      Params.Data (0) := Rose.Words.Word_32 (Current_Process_Id);
      Params.Data (1) := Rose.Words.Word (Virtual_Page);
      if Is_Mapped then
         Params.Data (2) :=
           Rose.Words.Word
             (Mapped_Physical_Page (Current_Process_Id, Virtual_Page));
      else
         Params.Data (2) := 0;
      end if;

      if Write_Attempt then
         Params.Data (3) := 1;
      elsif Execution_Attempt then
         Params.Data (3) := 2;
      end if;

      Send_Cap
        (From_Process => Current_Process_Id,
         To_Process   => Mem_Process,
         Sender_Cap   => 0,
         Receiver_Cap => Mem_Page_Fault_Cap,
         Params       => Params);

   end Handle_Page_Fault;

   -------------
   -- Has_Cap --
   -------------

   function Has_Cap
     (Process_Id : Rose.Objects.Process_Id;
      Cap        : Rose.Capabilities.Capability)
      return Boolean
   is
      use Rose.Capabilities.Layout;
      P : Kernel_Process_Entry renames Process_Table (Process_Id);
   begin
      return Cap in 1 .. Cached_Capability_Count
        and then P.Cap_Cache (Cap).Header.Cap_Type /= Null_Cap;
   end Has_Cap;

   ---------------------------
   -- Have_Process_Handlers --
   ---------------------------

   function Have_Process_Handlers return Boolean is
      use type Rose.Objects.Process_Id;
   begin
      return Mem_Process /= 0;
   end Have_Process_Handlers;

   ----------------------------
   -- Is_Blocked_On_Endpoint --
   ----------------------------

   function Is_Blocked_On_Endpoint
     (Process        : Rose.Objects.Process_Id;
      Endpoint_Index : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      if P.State /= Blocked then
         return False;
      end if;

      if P.Flags (Receive_Any) then
         return True;
      end if;

      --  FIXME: add support for cap pages
      if P.Receive_Cap not in P.Cap_Cache'Range then
         return False;
      end if;

      --  FIXME: add support for a cap set

      declare
         use type Rose.Capabilities.Layout.Capability_Type;
      begin
         if P.Cap_Cache (P.Receive_Cap).Header.Cap_Type
           /= Rose.Capabilities.Layout.Receive_Cap
         then
            return False;
         end if;
      end;

      declare
         use Rose.Objects;
         Endpoint : constant Rose.Objects.Endpoint_Index :=
                      P.Cap_Cache (P.Receive_Cap).Header.Endpoint;
         Blocked  : constant Boolean :=
                      Endpoint = 0
                          or else Endpoint = Endpoint_Index;
      begin
         if not Blocked then
            Rose.Boot.Console.Put ("receiver is blocked on ");
            Rose.Boot.Console.Put
              (Rose.Words.Word_8
                 (P.Cap_Cache (P.Receive_Cap).Header.Endpoint));
            Rose.Boot.Console.Put (" but we are sending to ");
            Rose.Boot.Console.Put
              (Rose.Words.Word_8
                 (Endpoint_Index));
            Rose.Boot.Console.New_Line;
         end if;

         return Blocked;
      end;

   end Is_Blocked_On_Endpoint;

   -----------------------------
   -- Is_Valid_Endpoint_Index --
   -----------------------------

   function Is_Valid_Endpoint_Index
     (Process        : Rose.Objects.Process_Id;
      Endpoint_Index : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      use type Rose.Objects.Endpoint_Id;
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      return Endpoint_Index in P.Endpoints'Range
        and then P.Endpoints (Endpoint_Index).Endpoint
        /= Rose.Objects.Null_Endpoint_Id;
   end Is_Valid_Endpoint_Index;

   --------------
   -- Map_Page --
   --------------

   procedure Map_Page
     (Process       : Rose.Objects.Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address;
      Physical_Page : Rose.Addresses.Physical_Page_Address;
      Readable      : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
      User          : Boolean)
   is
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      Rose.Kernel.Page_Table.Map_Page
        (Virtual_Page_Address
           (Rose.Addresses.Physical_Address_To_Page
                (P.Directory_Page
                 + Kernel_Virtual_Base)),
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
     (Process       : Rose.Objects.Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address)
      return Rose.Addresses.Physical_Page_Address
   is
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      return Rose.Kernel.Page_Table.Mapped_Physical_Page
        (Directory_Page =>
           Virtual_Page_Address
             (Rose.Addresses.Physical_Address_To_Page
                  (P.Directory_Page
                   + Kernel_Virtual_Base)),
         Virtual_Page   => Virtual_Page);
   end Mapped_Physical_Page;

   -------------------------
   -- Next_Blocked_Sender --
   -------------------------

   function Next_Blocked_Sender
     (Receiver   : Rose.Objects.Process_Id)
      return Rose.Objects.Process_Id
   is
      It   : constant Kernel_Process_Access :=
               Process_Table (Receiver).Waiting_First;
   begin
      if It /= null then
         Process_Table (Receiver).Waiting_First := It.Waiting_Next;
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
     (Receiver : Rose.Objects.Process_Id;
      Params   : Rose.Invocation.Invocation_Record)
   is
      P : Kernel_Process_Entry renames Process_Table (Receiver);
   begin
      P.Flags (Receive_Any) := False;
      P.Flags (Receive_Caps) := False;
      P.Current_Params := Params;
      P.Receive_Cap := Params.Cap;

      Set_Current_State (Receiver, Blocked);

   end Receive;

   -----------------
   -- Receive_Any --
   -----------------

   procedure Receive_Any
     (Receiver : Rose.Objects.Process_Id;
      Params   : Rose.Invocation.Invocation_Record)
   is
   begin
      Receive (Receiver, Params);
      Process_Table (Receiver).Flags (Receive_Any) := True;
   end Receive_Any;

   ----------------------------
   -- Report_Current_Process --
   ----------------------------

   procedure Report_Current_Process is
   begin
      Rose.Kernel.Processes.Debug.Report_Process
        (Current_Process.Pid);
   end Report_Current_Process;

   ------------------
   -- Return_Error --
   ------------------

   procedure Return_Error
     (Params : Rose.Invocation.Invocation_Access;
      Error  : Rose.Invocation.Invocation_Error)
   is
      Control : Rose.Invocation.Control_Word renames Params.Control;
   begin
      Control.Flags (Rose.Invocation.Error) := True;
      Control.Flags (Rose.Invocation.Reply) := True;
      Control.Flags (Rose.Invocation.Send) := True;
      Control.Flags (Rose.Invocation.Receive) := False;
      Control.Flags (Rose.Invocation.Block) := False;
      Current_Process.Current_Params.Data (1) :=
        Rose.Invocation.Invocation_Error'Pos (Error);
      Current_Process.Flags (Invoke_Reply) := True;
   end Return_Error;

   --------------
   -- Send_Cap --
   --------------

   procedure Send_Cap
     (From_Process : Rose.Objects.Process_Id;
      To_Process   : Rose.Objects.Process_Id;
      Sender_Cap   : Rose.Capabilities.Capability;
      Receiver_Cap : Rose.Capabilities.Capability;
      Params       : Rose.Invocation.Invocation_Record)
   is
      pragma Unreferenced (Sender_Cap);
      use Rose.Capabilities;
      use Rose.Invocation;
      use type Rose.Objects.Process_Id;
      From : Kernel_Process_Entry renames Process_Table (From_Process);
      To   : Kernel_Process_Entry renames Process_Table (To_Process);
   begin

      To.Current_Params := Params;
      To.Current_Params.Cap := Receiver_Cap;

      if Params.Control.Flags (Rose.Invocation.Send_Caps)
        and then From_Process /= 1
      then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            declare
               From_Cap : constant Capability := Params.Caps (Cap_Index);
               New_Cap  : constant Capability := Create_Cap (To_Process);
            begin
               if New_Cap = Null_Capability then
                  if Cap_Index > 0 then
                     for Deleted_Cap_Index in 0 .. Cap_Index - 1 loop
                        Delete_Cap
                          (To_Process,
                           To.Current_Params.Caps (Deleted_Cap_Index));
                     end loop;
                  end if;

                  Rose.Boot.Console.Put ("warning: process ");
                  Rose.Boot.Console.Put (Rose.Words.Word_8 (To_Process));
                  Rose.Boot.Console.Put (": out of capabilities");
                  Rose.Boot.Console.New_Line;

                  Rose.Invocation.Set_Error
                    (Params => From.Current_Params,
                     Error  => Rose.Invocation.Out_Of_Capabilities);
                  return;
               end if;

               To.Cap_Cache (New_Cap) := From.Cap_Cache (From_Cap);
               To.Current_Params.Caps (Cap_Index) := New_Cap;
               Rose.Kernel.Validation.Create_Cap
                 (To_Process, New_Cap, To.Cap_Cache (New_Cap).Header.Cap_Type);
            end;
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
               Share_Page (From_Process, To_Process, Page, Writable);
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

      if To_Process = Log_Process_Activity
        or else From_Process = Log_Process_Activity
      then
         Rose.Boot.Console.Put ("send-cap: ");
         Rose.Boot.Console.Put (To_Process);
         Rose.Boot.Console.Put (" ");
         Rose.Invocation.Trace.Put
           (To.Current_Params, False);
      end if;

      Set_Current_State (To_Process, Ready);

   end Send_Cap;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (From_Process : Rose.Objects.Process_Id;
      To_Process   : Rose.Objects.Process_Id;
      Params       : Rose.Invocation.Invocation_Record)
   is
      use Rose.Capabilities;
      use Rose.Invocation;
      use Rose.Objects;
      From : Kernel_Process_Entry renames Process_Table (From_Process);
      To   : Kernel_Process_Entry renames Process_Table (To_Process);
   begin
      if To_Process = 0 then
         return;
      end if;

      if To.State /= Blocked then
         return;
      end if;

      To.Current_Params := Params;

      if Params.Control.Flags (Rose.Invocation.Send_Caps) then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            declare
               From_Cap : constant Capability := Params.Caps (Cap_Index);
               New_Cap  : constant Capability := Create_Cap (To_Process);
            begin
               if New_Cap = Null_Capability then
                  if Cap_Index > 0 then
                     for Deleted_Cap_Index in 0 .. Cap_Index - 1 loop
                        Delete_Cap
                          (To_Process,
                           To.Current_Params.Caps (Deleted_Cap_Index));
                     end loop;
                  end if;

                  Rose.Boot.Console.Put ("warning: process ");
                  Rose.Boot.Console.Put (Rose.Words.Word_8 (To_Process));
                  Rose.Boot.Console.Put (": out of capabilities");
                  Rose.Boot.Console.New_Line;

                  Rose.Invocation.Set_Error
                    (Params => From.Current_Params,
                     Error  => Rose.Invocation.Out_Of_Capabilities);
                  return;
               end if;

               To.Cap_Cache (New_Cap) := From.Cap_Cache (From_Cap);
               To.Current_Params.Caps (Cap_Index) := New_Cap;
               Rose.Kernel.Validation.Create_Cap
                 (To_Process, New_Cap,
                  To.Cap_Cache (New_Cap).Header.Cap_Type);
            end;
         end loop;
      end if;

      Set_Current_State (To_Process, Ready);

   end Send_Reply;

   ----------------------
   -- Send_To_Endpoint --
   ----------------------

   procedure Send_To_Endpoint
     (From_Process    : Rose.Objects.Process_Id;
      To_Process      : Rose.Objects.Process_Id;
      Sender_Cap      : Rose.Capabilities.Capability;
      Endpoint        : Rose.Objects.Endpoint_Index;
      Identifier      : Rose.Objects.Capability_Identifier;
      Params          : Rose.Invocation.Invocation_Record)
   is
      use type Rose.Objects.Endpoint_Id;
      To   : Kernel_Process_Entry renames Process_Table (To_Process);
   begin
      if Endpoint in To.Endpoints'Range
        and then To.Endpoints (Endpoint).Endpoint /= 0
      then
         Send_Cap (From_Process, To_Process,
                   Sender_Cap, Sender_Cap,
                   (Params with delta
                      Endpoint => To.Endpoints (Endpoint).Endpoint,
                      Identifier => Identifier));
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
     (Process_Id : Rose.Objects.Process_Id;
      Cap        : Rose.Capabilities.Capability;
      Layout     : Rose.Capabilities.Layout.Capability_Layout)
   is
   begin
      Process_Table (Process_Id).Cap_Cache (Cap) := Layout;
   end Set_Cap;

   ----------------
   -- Set_Cap_Id --
   ----------------

   procedure Set_Cap_Id
     (Process      : Rose.Objects.Process_Id;
      Cap          : Rose.Capabilities.Capability;
      Id           : Rose.Objects.Capability_Identifier)
   is
   begin
      Process_Table (Process).Cap_Cache (Cap).Header.Identifier := Id;
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
     (Process : Rose.Objects.Process_Id;
      State   : Process_State)
   is
      Current_Process : Kernel_Process_Entry renames Process_Table (Process);
   begin
      if Log_State_Changes then
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Process));
         Rose.Boot.Console.Put (": state <- ");
         Rose.Boot.Console.Put
           (case State is
               when Ready       => "ready",
               when Blocked     => "blocked",
               when Interrupted => "interrupted",
               when Available   => "available",
               when Running     => "running",
               when Killed      => "killed");
         Rose.Boot.Console.New_Line;
      end if;

      if Current_Process.State = Killed then
         null;
--           Rose.Boot.Console.Put_Line
--             ("set state canceled; process has been killed");
      else
         Current_Process.Flags (Invoke_Reply) :=
           Current_Process.State = Blocked and then State = Ready;
         Current_Process.Flags (Interrupt_Resume) :=
           Current_Process.State = Interrupted and then State = Ready;

         Current_Process.State := State;
      end if;

   end Set_Current_State;

   -----------------------
   -- Set_Endpoint_Caps --
   -----------------------

   procedure Set_Endpoint_Caps
     (Process_Id  : Rose.Objects.Process_Id;
      Endpoint    : Rose.Objects.Endpoint_Index;
      Receive_Cap : Rose.Capabilities.Capability;
      Send_Cap    : Rose.Capabilities.Capability)
   is
      Current_Process : Kernel_Process_Entry renames
                          Process_Table (Process_Id);
      Rec             : Registered_Endpoint_Record renames
                          Current_Process.Endpoints (Endpoint);
   begin
      Rec.Receive_Cap := Receive_Cap;
      Rec.Send_Cap    := Send_Cap;
   end Set_Endpoint_Caps;

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

   ----------------
   -- Share_Page --
   ----------------

   procedure Share_Page
     (From_Process : Rose.Objects.Process_Id;
      To_Process   : Rose.Objects.Process_Id;
      Address      : Rose.Addresses.Virtual_Page_Address;
      Writable     : Boolean)
   is
      use Rose.Words;
      Physical_Page      : constant Physical_Page_Address :=
                                Mapped_Physical_Page
                                  (Process      => From_Process,
                                   Virtual_Page => Address);
      P                  : constant Kernel_Process_Access :=
                             Process_Table (To_Process)'Access;
      Virtual_Page       : constant Virtual_Page_Address :=
                             P.Invocation_Buffer;
   begin

      if Log_Shared_Buffers then
         Rose.Boot.Console.Put ("pid ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (From_Process));
         Rose.Boot.Console.Put (": share page with ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (To_Process));
         Rose.Boot.Console.New_Line;

         Rose.Boot.Console.Put (" from ");
         Rose.Boot.Console.Put (Rose.Words.Word (Address) * 4096);
         Rose.Boot.Console.Put (" [");
         Rose.Boot.Console.Put (Rose.Words.Word (Physical_Page) * 4096);
         Rose.Boot.Console.Put ("] to ");
         Rose.Boot.Console.Put (Rose.Words.Word (Virtual_Page) * 4096);
         Rose.Boot.Console.New_Line;
      end if;

      Map_Page
        (Process       => To_Process,
         Virtual_Page  => Virtual_Page,
         Physical_Page => Physical_Page,
         Readable      => True,
         Writable      => Writable,
         Executable    => False,
         User          => True);
      P.Invocation_Buffer := P.Invocation_Buffer + 1;
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

   ----------------------
   -- Unblock_And_Send --
   ----------------------

   procedure Unblock_And_Send
     (From_Process    : Rose.Objects.Process_Id;
      To_Process      : Rose.Objects.Process_Id;
      Receiver_Params : Rose.Invocation.Invocation_Access)
   is
      P : constant Kernel_Process_Access :=
            Process_Table (From_Process)'Access;
   begin
      Rose.Boot.Console.Put (To_Process);
      Rose.Boot.Console.Put (": processing next sender: ");
      Rose.Boot.Console.Put (From_Process);
      Rose.Boot.Console.New_Line;
      Rose.Invocation.Trace.Put
        (P.Current_Params, True);

      Send_To_Endpoint
        (From_Process => From_Process,
         To_Process   => To_Process,
         Sender_Cap   => P.Current_Params.Cap,
         Endpoint     => P.Waiting_Endpoint,
         Identifier   => P.Waiting_Cap_Id,
         Params       => P.Current_Params);
      Receiver_Params.all := Process_Table (To_Process).Current_Params;
   end Unblock_And_Send;

   -----------------------------
   -- Unmap_Invocation_Buffer --
   -----------------------------

   procedure Unmap_Invocation_Buffer
     (Process       : Rose.Objects.Process_Id)
   is
      use Rose.Words;
      P : Kernel_Process_Entry renames Process_Table (Process);
      Base : constant Virtual_Page_Address :=
               P.Page_Ranges (Invocation_Range_Index).Base;
   begin
      while P.Invocation_Buffer > Base loop
         P.Invocation_Buffer := P.Invocation_Buffer - 1;
         if Log_Shared_Buffers then
            Rose.Boot.Console.Put ("pid ");
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Process));
            Rose.Boot.Console.Put (": unmap buffer page ");
            Rose.Boot.Console.Put
              (Rose.Words.Word (P.Invocation_Buffer) * 4096);
            Rose.Boot.Console.New_Line;
         end if;

         Unmap_Page (Process, P.Invocation_Buffer);
      end loop;
   end Unmap_Invocation_Buffer;

   ----------------
   -- Unmap_Page --
   ----------------

   procedure Unmap_Page
     (Process       : Rose.Objects.Process_Id;
      Virtual_Page  : Rose.Addresses.Virtual_Page_Address)
   is
      P : Kernel_Process_Entry renames Process_Table (Process);
   begin
      Rose.Kernel.Page_Table.Unmap_Page
        (Directory_Page =>
           Virtual_Page_Address
             (Physical_Address_To_Page
                  (P.Directory_Page + Kernel_Virtual_Base)),
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
     (Waiting_Process   : Rose.Objects.Process_Id;
      Receiving_Process : Rose.Objects.Process_Id;
      Endpoint          : Rose.Objects.Endpoint_Index;
      Identifier        : Rose.Objects.Capability_Identifier;
      Params            : Rose.Invocation.Invocation_Record)
   is
      P : constant Kernel_Process_Access :=
            Process_Table (Waiting_Process)'Access;
      Q : constant Kernel_Process_Access :=
            Process_Table (Receiving_Process)'Access;
   begin
      P.State := Blocked;
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

end Rose.Kernel.Processes;
