with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with Ada.Unchecked_Conversion;

with Rose.Arch;
with Rose.Words;

with Rose.Kernel.Limits;
with Rose.Kernel.Heap;

with Rose.Kernel.Page_Table;

with Rose.Kernel.Processes.Queue;

with Rose.Kernel.Panic;

with Rose.Kernel.Capabilities.Arch;
with Rose.Kernel.Capabilities.Interrupt;
with Rose.Kernel.Capabilities.Kernel_Caps;
with Rose.Kernel.Capabilities.Meta;
with Rose.Kernel.Capabilities.Physical_Memory;

with Rose.Init_Capabilities;

with Rose.Formats.ELF;
with Rose.Interfaces.Memory;

with Rose.Boot.Console;
with Rose.Arch.Interrupts;

package body Rose.Kernel.Processes.Init is

   package Process_Table_Conversions is
     new System.Address_To_Access_Conversions (Kernel_Process_Table);

   Enable_Paging : constant Boolean := True;

   Empty_Environment : aliased Rose.Environment_Pages.Environment_Page;

   procedure Add_Init_Caps (Pid : Process_Id);
   --  Give init the caps listed in Rose.Init_Capabilities, at the indices
   --  listed there.

   -------------------
   -- Add_Init_Caps --
   -------------------

   procedure Add_Init_Caps (Pid : Process_Id) is
      use Rose.Capabilities.Layout;
      use type Rose.Capabilities.Capability;
      use type Rose.Objects.Object_Id;
      package Init_Caps renames Rose.Init_Capabilities;
      package Kernel_Caps renames Rose.Kernel.Capabilities.Kernel_Caps;
      package Meta renames Rose.Kernel.Capabilities.Meta;
      package Memory renames Rose.Kernel.Capabilities.Physical_Memory;

      function Layout
        (Cap_Type : Capability_Type;
         Endpoint : Rose.Objects.Endpoint_Index;
         Payload  : Rose.Objects.Object_Id := 0)
         return Capability_Layout
      is ((Cap_Type => Cap_Type, Endpoint => Endpoint, others => <>),
          Payload);

      function Kernel (Endpoint : Rose.Objects.Endpoint_Index)
                       return Capability_Layout
      is (Layout (Kernel_Cap, Endpoint));

      Layouts : constant array
        (Rose.Capabilities.Capability range 1 .. Init_Caps.Last_Cap)
        of Capability_Layout :=
          (Init_Caps.Create_Cap             => Layout (Create_Cap, 0),
           Init_Caps.Boot_Cap               => Layout (Boot_Cap, 1),
           Init_Caps.Reserve_Memory_Cap     =>
             Layout (Physical_Memory_Cap, Memory.Reserve_Device_Memory),
           Init_Caps.Region_Count_Cap       =>
             Layout (Physical_Memory_Cap, Memory.Get_Region_Count),
           Init_Caps.Get_Region_Cap         =>
             Layout (Physical_Memory_Cap, Memory.Get_Region),
           Init_Caps.Create_Endpoint_Cap    =>
             Layout (Meta_Cap, Meta.Create_Endpoint_Endpoint),
           Init_Caps.Rescind_Cap            =>
             Layout (Meta_Cap, Meta.Rescind_Cap),
           Init_Caps.Delete_Cap             =>
             Layout (Meta_Cap, Meta.Delete_Cap),
           Init_Caps.Exit_Cap               =>
             Layout (Meta_Cap, Meta.Exit_Process),
           Init_Caps.Page_On_Cap            =>
             Kernel (Kernel_Caps.Enable_Paging_Endpoint),
           Init_Caps.Write_System_Image_Cap =>
             Kernel (Kernel_Caps.Start_Checkpoint_Endpoint),
           Init_Caps.Create_Process_Cap     =>
             Kernel (Kernel_Caps.Create_Process_Endpoint),
           Init_Caps.Set_Timeout_Cap        =>
             Kernel (Kernel_Caps.Set_Timeout_Endpoint),
           Init_Caps.Get_Ticks_Cap          =>
             Kernel (Kernel_Caps.Get_Current_Ticks_Endpoint),
           Init_Caps.Enter_Checkpoint_Cap   =>
             Kernel (Kernel_Caps.Enter_Checkpoint_Endpoint),
           Init_Caps.Leave_Checkpoint_Cap   =>
             Kernel (Kernel_Caps.Leave_Checkpoint_Endpoint),
           Init_Caps.Start_Log_Cap          =>
             Kernel (Kernel_Caps.Start_Log_Endpoint),
           Init_Caps.Trace_Cap              =>
             Kernel (Kernel_Caps.Trace_Endpoint),
           Init_Caps.Restore_Process_Cap    =>
             Kernel (Kernel_Caps.Restore_Process_Endpoint),
           Init_Caps.Resume_Process_Cap     =>
             Kernel (Kernel_Caps.Resume_Process_Endpoint),
           Init_Caps.Restore_Interrupt_Cap  =>
             Kernel (Kernel_Caps.Restore_Interrupt_Endpoint),
           Init_Caps.Ports_Cap              =>
             Layout (Arch_Cap,
                     Rose.Objects.Endpoint_Index
                       (Rose.Kernel.Capabilities.Arch.Derive_Endpoint),
                     16#FFFF# * 2 ** 32),
           Init_Caps.Interrupts_Cap         =>
             Layout (Interrupt_Cap,
                     Rose.Kernel.Capabilities.Interrupt.Derive_Endpoint,
                     Init_Caps.First_Device_Vector
                     + Init_Caps.Last_Device_Vector * 2 ** 32),
           Init_Caps.Device_Pages_Cap       =>
             Layout (Physical_Memory_Cap, Memory.Device_Page));

   begin
      for Index in Layouts'Range loop
         declare
            Cap : constant Rose.Capabilities.Capability := Create_Cap (Pid);
         begin
            if Cap /= Index then
               Rose.Kernel.Panic.Panic ("init: unexpected cap index");
            end if;
            Set_Cap (Pid, Cap, Layouts (Index));
         end;
      end loop;

      Rose.Boot.Console.Put ("init: added ");
      Rose.Boot.Console.Put (Natural (Init_Caps.Last_Cap));
      Rose.Boot.Console.Put (" caps");
      Rose.Boot.Console.New_Line;
   end Add_Init_Caps;

   ------------------------
   -- Init_Process_Table --
   ------------------------

   procedure Init_Process_Table is
      Table_Address : constant Virtual_Address :=
                        Rose.Kernel.Heap.Allocate
                          (Count => Rose.Kernel.Limits.Max_Processes
                           * Kernel_Process_Entry'Size / 8,
                           Align => 4);
      function To_Word is
        new Ada.Unchecked_Conversion (System.Address, Rose.Words.Word);
      Idle          : Rose.Words.Word;
      pragma Import (C, Idle, "idle");
   begin
      Process_Table :=
        Process_Table_Access
          (Process_Table_Conversions.To_Pointer
             (System'To_Address (Table_Address)));

      for I in Process_Table'Range loop
         declare
            P : Kernel_Process_Entry renames Process_Table (I);
         begin
            P.Pid := I;
            P.Oid := Rose.Objects.Null_Object_Id;
            P.State := Available;
            P.Name := (others => ' ');
            P.Shared_Pages := (others => (0, 0));
         end;
      end loop;

      Current_Process := Process_Table (1)'Access;
      Current_Process.Flags (Boot_Module) := True;

      Current_Process.Stack :=
        Rose.Kernel.Arch.Process_Stack_Frame
          (Start_EIP       => To_Word (Idle'Address),
           Privilege_Level => 0,
           Start_PSW       => 16#1200#,
           Code_Segment    => 16#08#,
           Data_Segment    => 16#10#);
      Current_Process.Directory_Page :=
        Rose.Kernel.Page_Table.Kernel_Page_Directory;

      Current_Process.Priority := Process_Priority'Last;
      Current_Process.State := Ready;
      Current_Process.Name (1 .. 4) := "idle";
      Current_Process.Oid := Rose.Objects.Idle_Object_Id;
      Rose.Kernel.Processes.Queue.Queue_Process (1);

      declare
         Pid : constant Rose.Kernel.Processes.Process_Id :=
                 Load_Boot_Module
                   (Priority    => Process_Priority'Last,
                    Module      => Rose.Kernel.Modules.Init_Module,
                    Oid         => Rose.Objects.Init_Object_Id,
                    Environment => null);
      begin
         pragma Unreferenced (Pid);
      end;

      Rose.Kernel.Interrupts.Set_Handler
        (Rose.Arch.Interrupts.General_Protection_Fault,
         Handle_General_Protection_Fault'Access);

      Queue.Choose_Process;

   end Init_Process_Table;

   ----------------------
   -- Load_Boot_Module --
   ----------------------

   function Load_Boot_Module
     (Priority    : Process_Priority;
      Module      : Rose.Kernel.Modules.Module_Index;
      Oid         : Rose.Objects.Boot_Process_Object_Id;
      Environment : access Rose.Environment_Pages.Environment_Page)
      return Rose.Kernel.Processes.Process_Id
   is
      use Rose.Invocation;
      Base              : System.Address;
      Length            : System.Storage_Elements.Storage_Count;
      Name              : Process_Name;
      Name_Length       : Natural;
      Pid               : Process_Id := 1;
      Launch_Params     : Rose.Invocation.Invocation_Record;
   begin

      while Process_Table (Pid).State /= Available loop
         Pid := Pid + 1;
      end loop;

      Rose.Kernel.Modules.Get_Module_Name (Module, Name, Name_Length);
      Rose.Kernel.Modules.Get_Module_Image (Module, Base, Length);

      declare
         use Rose.Formats.ELF;
         Image                   : constant Elf_Image := Load (Base, Length);
         Proc                    : constant Kernel_Process_Access :=
                                     Process_Table (Pid)'Access;
         Directory_VP            : Virtual_Page_Address;
         Physical_Directory_Page : Physical_Page_Address;

         Heap_Pages : Natural := 0;
         --  The kernel heap pages that loading the module takes
         Last_Table : Natural := Natural'Last;
         --  The page table that maps the last segment counted

         procedure Count_Program_Header
           (Segment_Type : Program_Header_Type;
            File_Offset  : Rose.Formats.ELF.Elf_Addr;
            V_Address    : Rose.Formats.ELF.Elf_Addr;
            File_Size    : Rose.Words.Word_32;
            Memory_Size  : Rose.Words.Word_32;
            Alignment    : Rose.Words.Word_32;
            Readable     : Boolean;
            Writable     : Boolean;
            Executable   : Boolean);

         --------------------------
         -- Count_Program_Header --
         --------------------------

         procedure Count_Program_Header
           (Segment_Type : Program_Header_Type;
            File_Offset  : Rose.Formats.ELF.Elf_Addr;
            V_Address    : Rose.Formats.ELF.Elf_Addr;
            File_Size    : Rose.Words.Word_32;
            Memory_Size  : Rose.Words.Word_32;
            Alignment    : Rose.Words.Word_32;
            Readable     : Boolean;
            Writable     : Boolean;
            Executable   : Boolean)
         is
            pragma Unreferenced (File_Offset, Alignment,
                                 Readable, Executable);
            use type Rose.Words.Word_32;
            Page_Bytes  : constant := Virtual_Page_Bytes;
            Table_Bytes : constant := 1024 * Page_Bytes;
            First_Table : constant Natural :=
                            Natural (V_Address / Table_Bytes);
            Final_Table : constant Natural :=
                            Natural
                              ((V_Address
                               + Rose.Words.Word_32'Max (Memory_Size, 1) - 1)
                               / Table_Bytes);
         begin
            if Segment_Type /= PT_LOAD then
               return;
            end if;

            if Writable then
               declare
                  Size : constant Rose.Words.Word_32 :=
                           (if Have_Process_Handlers and then Enable_Paging
                            then File_Size else Memory_Size);
               begin
                  Heap_Pages := Heap_Pages
                    + Natural ((Size + Page_Bytes - 1) / Page_Bytes);
               end;
            end if;

            for Table in First_Table .. Final_Table loop
               if Table /= Last_Table then
                  Heap_Pages := Heap_Pages + 1;
                  Last_Table := Table;
               end if;
            end loop;
         end Count_Program_Header;

         procedure Load_Program_Header
           (Segment_Type : Program_Header_Type;
            File_Offset  : Rose.Formats.ELF.Elf_Addr;
            V_Address    : Rose.Formats.ELF.Elf_Addr;
            File_Size    : Rose.Words.Word_32;
            Memory_Size  : Rose.Words.Word_32;
            Alignment    : Rose.Words.Word_32;
            Readable     : Boolean;
            Writable     : Boolean;
            Executable   : Boolean);

         -------------------------
         -- Load_Program_Header --
         -------------------------

         procedure Load_Program_Header
           (Segment_Type : Program_Header_Type;
            File_Offset  : Rose.Formats.ELF.Elf_Addr;
            V_Address    : Rose.Formats.ELF.Elf_Addr;
            File_Size    : Rose.Words.Word_32;
            Memory_Size  : Rose.Words.Word_32;
            Alignment    : Rose.Words.Word_32;
            Readable     : Boolean;
            Writable     : Boolean;
            Executable   : Boolean)
         is
            use type Rose.Words.Word_32;
            File_Addr : constant Virtual_Address :=
                          To_Virtual_Address (Base)
                          + Virtual_Bytes (File_Offset);
            Phys_Addr : Physical_Address;
            Virt_Addr : constant Virtual_Address :=
                          Virtual_Address (V_Address);
            Heap_Addr : Virtual_Address;
            Phys_Page : Physical_Page_Address;
            Virt_Page : Virtual_Page_Address;
            Allocated : Rose.Words.Word_32 := 0;
            Prealloc  : Rose.Words.Word_32 := 0;
            Range_Index : Page_Range_Index;
            Word_Index  : Parameter_Word_Index;
         begin
            case Segment_Type is
               when PT_LOAD =>
                  if not Writable then
                     Phys_Addr :=
                       Physical_Address
                         (File_Addr - Kernel_Virtual_Base);
                     Range_Index := Text_Range_Index;
                  else
                     if Have_Process_Handlers
                       and then Enable_Paging
                     then
                        --  we can page
                        Heap_Addr :=
                          Rose.Kernel.Heap.Allocate
                            (Align_Up_To_Page_Boundary
                               (Virtual_Bytes (File_Size)),
                             Virtual_Page_Bytes);

                        Rose.Arch.Copy_Memory
                          (File_Addr, Heap_Addr, Virtual_Bytes (File_Size));

                        Phys_Addr :=
                          Rose.Kernel.Heap.Get_Physical_Address (Heap_Addr);

                     else
                        --  we need to allocate kernel heap
                        Heap_Addr :=
                          Rose.Kernel.Heap.Allocate
                            (Virtual_Bytes (Memory_Size),
                             Virtual_Bytes (Alignment));
                        Rose.Arch.Copy_Memory
                          (File_Addr, Heap_Addr, Virtual_Bytes (File_Size));

                        Phys_Addr :=
                          Rose.Kernel.Heap.Get_Physical_Address
                            (Heap_Addr);
                     end if;
                     Range_Index := Data_Range_Index;
                  end if;

                  Phys_Page := Physical_Address_To_Page (Phys_Addr);
                  Virt_Page :=
                    Virtual_Address_To_Page (Virt_Addr);

                  if Executable then
                     Word_Index := 0;
                  elsif Writable then
                     Word_Index := 4;
                  else
                     Word_Index := 2;
                  end if;

                  Launch_Params.Data (Word_Index) :=
                    Rose.Words.Word (Virt_Page);
                  Launch_Params.Data (Word_Index + 1) :=
                    Rose.Words.Word (Virt_Page)
                    + Rose.Words.Word
                    (Align_Up_To_Page_Boundary
                       (Rose.Addresses.Physical_Address (Memory_Size)))
                      / Virtual_Page_Bytes;

                  if Executable then
                     Proc.Code_Page := Phys_Page;
                     Range_Index := Exec_Range_Index;
                  else
                     Proc.Data_Page := Phys_Page;
                  end if;

                  if not Writable
                    or else not Have_Process_Handlers
                    or else not Enable_Paging
                  then
                     Prealloc := Memory_Size;
                  else
                     Prealloc := File_Size;
                  end if;

                  while Allocated < Prealloc loop

                     if not Proc.Page_Flags (Range_Index).Valid then
                        Proc.Page_Flags (Range_Index).Valid := True;
                        Proc.Page_Flags (Range_Index).Readable := True;
                        Proc.Page_Flags (Range_Index).Writable :=
                          Range_Index = Data_Range_Index;
                        Proc.Page_Flags (Range_Index).Executable :=
                          Range_Index = Exec_Range_Index;
                        Proc.Page_Ranges (Range_Index).Base := Virt_Page;
                     end if;

                     Rose.Kernel.Page_Table.Map_Page
                       (Directory_Page => Directory_VP,
                        Virtual_Page   => Virt_Page,
                        Physical_Page  => Phys_Page,
                        Readable       => Readable,
                        Writable       => Writable,
                        Executable     => Executable,
                        User           => True);
                     Virt_Page := Virt_Page + 1;
                     Phys_Page := Phys_Page + 1;
                     Allocated := Allocated + Physical_Page_Bytes;
                     Proc.Page_Ranges (Range_Index).Bound := Virt_Page;
                  end loop;
               when others =>
                  null;
            end case;
         end Load_Program_Header;

      begin
         if not Is_Valid (Image) then
            Rose.Kernel.Panic.Panic ("boot module: not an ELF image");
         end if;

         --  a page directory, a stack page and its page table, an
         --  environment page and its page table, and a page table for
         --  the system call page
         Heap_Pages := (if Environment = null then 5 else 6);
         Rose.Formats.ELF.Scan_Program_Headers
           (Image, Count_Program_Header'Access);

         if Physical_Bytes (Heap_Pages) * Physical_Page_Bytes
           > Rose.Kernel.Heap.Available_Heap
         then
            Rose.Boot.Console.Enable_Display;
            Rose.Boot.Console.Put ("kernel: loading ");
            Rose.Boot.Console.Put (Name (1 .. Name_Length));
            Rose.Boot.Console.Put (" needs ");
            Rose.Boot.Console.Put (Heap_Pages);
            Rose.Boot.Console.Put (" pages of kernel heap; ");
            Rose.Boot.Console.Put
              (Natural (Rose.Kernel.Heap.Available_Heap
               / Physical_Page_Bytes));
            Rose.Boot.Console.Put (" are left (boot-module-pages=");
            Rose.Boot.Console.Put (Boot_Module_Pages);
            Rose.Boot.Console.Put (")");
            Rose.Boot.Console.New_Line;
            Rose.Kernel.Panic.Panic ("boot module heap exhausted");
         end if;

         Proc.Oid := Oid;
         Proc.Flags (Boot_Module) := True;

         declare
            use Rose.Objects;
         begin
            if Trace_Object_Id = Proc.Oid then
               Proc.Flags (Trace) := True;
            end if;
         end;

         Proc.Stack :=
           Rose.Kernel.Arch.Process_Stack_Frame
             (Start_EIP => Start_Address (Image));

         Proc.Priority := Priority;
         Proc.State := Ready;
         Proc.Quantum_Ticks := 10;
         Proc.Remaining_Ticks := 10;

         for I in 1 .. Proc.Name'Last loop
            if I <= Name_Length then
               Proc.Name (I) := Name (I);
            else
               Proc.Name (I) := ' ';
            end if;
         end loop;

         Proc.Page_Flags := (others => (others => False));
         Proc.Page_Ranges := (others => (others => 0));

         Directory_VP := Rose.Kernel.Heap.Allocate_Page;
         Physical_Directory_Page :=
           Rose.Kernel.Heap.Get_Physical_Page (Directory_VP);

         Rose.Kernel.Page_Table.Init_User_Page_Directory (Directory_VP);

         Rose.Formats.ELF.Scan_Program_Headers
           (Image, Load_Program_Header'Access);

         Proc.Stack_Page := Rose.Kernel.Heap.Allocate_Page;

         if Environment = null then
            Proc.Env_Page :=
              Rose.Kernel.Heap.Get_Physical_Page
                (Virtual_Address_To_Page
                   (To_Virtual_Address (Empty_Environment'Address)));
         else
            Proc.Env_Page := Rose.Kernel.Heap.Allocate_Page;
         end if;

         declare
            use Rose.Kernel.Page_Table;
            System_Call_Page : constant Physical_Page_Address :=
                                 Rose.Kernel.Heap.Get_Physical_Page
                                   (Virtual_Address_To_Page
                                      (To_Virtual_Address
                                         (System_Call_Entry'Address)));
         begin
            Map_Page
              (Directory_Page => Directory_VP,
               Virtual_Page   =>
                 Virtual_Address_To_Page (Process_Stack_Bound - 1),
               Physical_Page  => Proc.Stack_Page,
               Readable       => True,
               Writable       => True,
               Executable     => False,
               User           => True);
            Proc.Page_Ranges (Stack_Range_Index) :=
              (Virtual_Address_To_Page (Process_Stack_Bound - 16#1_0000#),
               Virtual_Address_To_Page (Process_Stack_Bound));
            Proc.Page_Flags (Stack_Range_Index) :=
              (Valid => True, Readable => True, Writable => True,
               others => False);

            Map_Page
              (Directory_Page => Directory_VP,
               Virtual_Page   =>
                 Virtual_Address_To_Page (Environment_Base),
               Physical_Page  => Proc.Env_Page,
               Readable       => True,
               Writable       => Environment /= null,
               Executable     => False,
               User           => True);

            if Environment /= null then
               Map_Kernel_Page
                 (Virtual_Page  =>
                    Rose.Kernel.Heap.Get_Virtual_Page (Proc.Env_Page),
                  Physical_Page => Proc.Env_Page,
                  Readable      => True,
                  Writable      => True,
                  Executable    => False,
                  User          => False);

               declare
                  Kernel_Page_Address : constant Virtual_Address :=
                                          Virtual_Address
                                            (Rose.Kernel.Heap.Get_Virtual_Page
                                               (Proc.Env_Page))
                                          * 4096;
                  Kernel_Env_Page   : Rose.Environment_Pages.Environment_Page;
                  pragma Import (Ada, Kernel_Env_Page);
                  for Kernel_Env_Page'Address use
                    System'To_Address (Kernel_Page_Address);
               begin
                  Kernel_Env_Page := Environment.all;
               end;
            end if;

            Launch_Params.Data (6) :=
              Rose.Words.Word (Proc.Page_Ranges (Stack_Range_Index).Base);
            Launch_Params.Data (7) :=
              Rose.Words.Word (Proc.Page_Ranges (Stack_Range_Index).Bound);

            Map_Page
              (Directory_Page => Directory_VP,
               Virtual_Page   =>
                 Virtual_Address_To_Page (16#FFFF_0000#),
               Physical_Page  => System_Call_Page,
               Readable       => True,
               Writable       => False,
               Executable     => True,
               User           => True);

            if Pid = 2 then

               Add_Init_Caps (Pid);

               if Use_Serial_Port then
                  Rose.Boot.Console.Put_Line
                    ("Switching to serial port");
                  Rose.Boot.Console.Disable_Display;
               end if;

            end if;
         end;

         Proc.Page_Ranges (Invocation_Range_Index) :=
           (Virtual_Address_To_Page (Invocation_Buffer_Range_Base),
            Virtual_Address_To_Page (Invocation_Buffer_Range_Bound));
         Proc.Shared_Pages (1).Page :=
           Proc.Page_Ranges (Invocation_Range_Index).Base;

         Proc.Directory_Page :=
           Physical_Page_To_Address (Physical_Directory_Page);

         if Have_Process_Handlers then
            Launch_Params.Cap := Mem_Launch_Cap;
            Launch_Params.Endpoint :=
              Rose.Interfaces.Memory.Register_Process_Endpoint;
            Launch_Params.Control.Last_Sent_Word := 7;
            Launch_Params.Control.Flags (Rose.Invocation.Send_Words) := True;

            declare
               use Rose.Capabilities.Layout;
               Process_Cap : constant Rose.Capabilities.Capability :=
                               Rose.Kernel.Processes.Create_Cap
                                 (Mem_Process);
            begin
               Rose.Kernel.Processes.Set_Cap
                 (Mem_Process, Process_Cap,
                  Capability_Layout'
                    (Header  =>
                         (Cap_Type => Rose.Capabilities.Layout.Process_Cap,
                          Endpoint => 1,
                          others   => <>),
                     Payload => To_Object_Id (Pid)));
               Send_Cap (Launch_Params, Process_Cap);
            end;

            Send_Cap
              (From_Process_Id => 1,
               To_Process_Id   => Mem_Process,
               Sender_Cap      => 0,
               Receiver_Cap    => Mem_Launch_Cap,
               Params          => Launch_Params);
         end if;

         Rose.Kernel.Processes.Queue.Queue_Process (Pid);

         Rose.Boot.Console.Put ("started boot module process ");
         Rose.Boot.Console.Put (Process_Table (Pid).Name);
         Rose.Boot.Console.New_Line;

      end;

      return Pid;

   end Load_Boot_Module;

end Rose.Kernel.Processes.Init;
