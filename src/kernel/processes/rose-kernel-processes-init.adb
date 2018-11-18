with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with Ada.Unchecked_Conversion;

with Rose.Arch;
with Rose.Words;

with Rose.Kernel.Limits;
with Rose.Kernel.Heap;

with Rose.Kernel.Page_Table;

with Rose.Kernel.Processes.Queue;
with Rose.Kernel.Validation;

with Rose.Kernel.Panic;

with Rose.Formats.ELF;

with Rose.Boot.Console;
with Rose.Arch.Interrupts;

package body Rose.Kernel.Processes.Init is

--     Init_Name    : aliased constant String := "init";
--     Console_Name : aliased constant String := "console";
--     MM_Name      : aliased constant String := "mm";
--     Unknown_Name : aliased constant String := "unknown";

   package Process_Table_Conversions is
     new System.Address_To_Access_Conversions (Kernel_Process_Table);

   Launch_Params : aliased Rose.Invocation.Invocation_Record;
   Enable_Paging : constant Boolean := True;

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
         Process_Table (I).Pid := I;
         Process_Table (I).State := Available;
         Process_Table (I).Name := (others => ' ');
      end loop;

      Current_Process := Process_Table (1)'Access;

      Process_Table (1).Stack :=
        Rose.Kernel.Arch.Process_Stack_Frame
          (Start_EIP       => To_Word (Idle'Address),
           Privilege_Level => 0,
           Start_PSW       => 16#1200#,
           Code_Segment    => 16#08#,
           Data_Segment    => 16#10#);
      Process_Table (1).Directory_Page :=
        Rose.Kernel.Page_Table.Kernel_Page_Directory;

      Process_Table (1).Priority := Process_Priority'Last;
      Process_Table (1).State := Ready;
      Process_Table (1).Name (1 .. 4) := "idle";
      Rose.Kernel.Processes.Queue.Queue_Process (1);

      declare
         Pid : constant Rose.Objects.Process_Id :=
                 Load_Boot_Module
                   (Priority   => 15,
                    Module     => Rose.Kernel.Modules.Init_Module);
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
     (Priority   : Process_Priority;
      Module     : Rose.Kernel.Modules.Module_Index)
      return Rose.Objects.Process_Id
   is
      use Rose.Invocation;
      use Rose.Objects;
      Base              : System.Address;
      Length            : System.Storage_Elements.Storage_Count;
      Launch_Index      : Parameter_Word_Index := 0;

      --        Name   : access constant String;
      Pid          : Process_Id := 1;
   begin

      while Process_Table (Pid).State /= Available loop
         Pid := Pid + 1;
      end loop;

      Rose.Kernel.Modules.Get_Module_Image (Module, Base, Length);

      declare
         use Rose.Formats.ELF;
         Image                   : constant Elf_Image := Load (Base, Length);
         Proc                    : constant Kernel_Process_Access :=
                                     Process_Table (Pid)'Access;
         Directory_VP            : Virtual_Page_Address;
         Physical_Directory_Page : Physical_Page_Address;

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
                          Physical_Address (Heap_Addr - Kernel_Virtual_Base);

                     else
                        --  we need to allocate kernel heap
                        Heap_Addr :=
                          Rose.Kernel.Heap.Allocate
                            (Virtual_Bytes (Memory_Size),
                             Virtual_Bytes (Alignment));
                        Rose.Arch.Copy_Memory
                          (File_Addr, Heap_Addr, Virtual_Bytes (File_Size));
                        Phys_Addr :=
                          Physical_Address (Heap_Addr - Kernel_Virtual_Base);
                     end if;
                     Range_Index := Data_Range_Index;
                  end if;

                  Phys_Page := Physical_Address_To_Page (Phys_Addr);
                  Virt_Page :=
                    Virtual_Address_To_Page (Virt_Addr);

                  Launch_Index := Launch_Index + 1;
                  Launch_Params.Data (Launch_Index) :=
                    Boolean'Pos (Readable)
                    + 2 * Boolean'Pos (Writable)
                    + 4 * Boolean'Pos (Executable);
                  Launch_Index := Launch_Index + 1;
                  Launch_Params.Data (Launch_Index) :=
                    Rose.Words.Word (Virt_Page);
                  Launch_Index := Launch_Index + 1;
                  Launch_Params.Data (Launch_Index) :=
                    Rose.Words.Word (Virt_Page)
                    + Rose.Words.Word
                    (Align_Up_To_Page_Boundary
                       (Rose.Addresses.Physical_Address (Memory_Size))
                     / Virtual_Page_Bytes);

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

         Launch_Params.Data (Launch_Index) :=
           Rose.Words.Word (Pid);

         Proc.Stack :=
           Rose.Kernel.Arch.Process_Stack_Frame
             (Start_EIP => Start_Address (Image));

         Proc.Priority := Priority;
         Proc.State := Ready;

         case Pid is
            when 2 =>
               Proc.Name (1 .. 4) := "init";
            when 3 =>
               Proc.Name (1 .. 7) := "console";
            when 4 =>
               Proc.Name (1 .. 3) := "mem";
            when 5 =>
               Proc.Name (1 .. 3) := "pci";
            when 6 =>
               Proc.Name (1 .. 3) := "ata";
            when 7 =>
               Proc.Name (1 .. 7) := "restore";
            when others =>
               Proc.Name (1 .. 5) := "boot ";
               Proc.Name (6) := Character'Val (Pid + 48);
         end case;

         Proc.Page_Flags := (others => (others => False));
         Proc.Page_Ranges := (others => (others => 0));

         Directory_VP := Rose.Kernel.Heap.Allocate_Page;
         Physical_Directory_Page :=
           Physical_Page_Address
             (Directory_VP - Kernel_Virtual_Page_Base);

         Rose.Kernel.Page_Table.Init_User_Page_Directory (Directory_VP);

         Rose.Formats.ELF.Scan_Program_Headers
           (Image, Load_Program_Header'Access);

         Proc.Stack_Page := Rose.Kernel.Heap.Allocate_Page;

         declare
            use Rose.Kernel.Page_Table;
            System_Call_Page : constant Physical_Page_Address :=
                                 Physical_Address_To_Page
                                   (To_Kernel_Physical_Address
                                      (To_Virtual_Address
                                         (System_Call_Entry'Address)));

         begin
            Map_Page
              (Directory_Page => Directory_VP,
               Virtual_Page   =>
                 Virtual_Address_To_Page (16#BFFF_FFF0#),
               Physical_Page  => Proc.Stack_Page,
               Readable       => True,
               Writable       => True,
               Executable     => False,
               User           => True);
            Proc.Page_Ranges (Stack_Range_Index) :=
              (Virtual_Address_To_Page (16#BFFF_0000#),
               Virtual_Address_To_Page (16#C000_0000#));
            Proc.Page_Flags (Stack_Range_Index) :=
              (Valid => True, Readable => True, Writable => True,
               others => False);

            Launch_Index := Launch_Index + 1;
            Launch_Params.Data (Launch_Index) := 3;  --  stack is r/w
            Launch_Index := Launch_Index + 1;
            Launch_Params.Data (Launch_Index) :=
              Rose.Words.Word (Proc.Page_Ranges (Stack_Range_Index).Base);
            Launch_Index := Launch_Index + 1;
            Launch_Params.Data (Launch_Index) :=
              Rose.Words.Word (Proc.Page_Ranges (Stack_Range_Index).Bound);

            Map_Page
              (Directory_Page => Directory_VP,
               Virtual_Page   =>
                 Virtual_Address_To_Page (16#F000_0000#),
               Physical_Page  => System_Call_Page,
               Readable       => True,
               Writable       => False,
               Executable     => True,
               User           => True);

            if Pid = 2 then

               Rose.Boot.Console.Put_Line
                 ("init: adding create cap");

               declare
                  use Rose.Capabilities.Layout;
               begin
                  Proc.Cap_Cache (1) :=
                    Generic_Capability_Layout'
                      (Header => Generic_Capability_Header'
                         (Cap_Type    => Create_Cap,
                          others      => <>),
                       Payload => 0);
                  Rose.Kernel.Validation.Create_Cap (2, 1, Create_Cap);
               end;

               if Use_Serial_Port then
                  Rose.Boot.Console.Put_Line
                    ("Switching to serial port");
                  Rose.Boot.Console.Enable_Serial_Port;
               end if;

            end if;
         end;

         Proc.Page_Ranges (Invocation_Range_Index) :=
           (Virtual_Address_To_Page (Invocation_Buffer_Range_Base),
            Virtual_Address_To_Page (Invocation_Buffer_Range_Bound));
         Proc.Invocation_Buffer :=
           Proc.Page_Ranges (Invocation_Range_Index).Base;

         Proc.Directory_Page :=
           Physical_Page_To_Address (Physical_Directory_Page);

         if Have_Process_Handlers then
            Launch_Params.Cap := Mem_Launch_Cap;
            Launch_Params.Control.Last_Sent_Word := Launch_Index;
            Launch_Params.Control.Last_Sent_Cap := 1;
            Launch_Params.Control.Flags :=
              (Send       => True,
               Send_Words => True,
               Send_Caps  => True,
               others     => False);
            Launch_Params.Endpoint := 100;

            declare
               use Rose.Capabilities.Layout;
               Resume_Cap : constant Rose.Capabilities.Capability :=
                              Rose.Kernel.Processes.Create_Cap
                                (Mem_Process);
            begin
               Rose.Kernel.Processes.Set_Cap
                 (Mem_Process, Resume_Cap,
                  Generic_Capability_Layout'
                    (Header  =>
                         (Cap_Type => Rose.Capabilities.Layout.Process_Cap,
                          Endpoint => 1,
                          others   => <>),
                     Payload => Rose.Objects.To_Object_Id (Pid)));
               Rose.Kernel.Validation.Create_Cap
                 (Mem_Process, Resume_Cap,
                  Rose.Capabilities.Layout.Process_Cap);
               Launch_Params.Caps (0) := Resume_Cap;
            end;

            declare
               use Rose.Capabilities.Layout;
               Faulted_Cap : constant Rose.Capabilities.Capability :=
                               Rose.Kernel.Processes.Create_Cap
                                 (Mem_Process);
            begin
               Rose.Kernel.Processes.Set_Cap
                 (Mem_Process, Faulted_Cap,
                  Generic_Capability_Layout'
                    (Header  =>
                         (Cap_Type => Rose.Capabilities.Layout.Process_Cap,
                          Endpoint => 2,
                          others   => <>),
                     Payload => Rose.Objects.To_Object_Id (Pid)));
               Rose.Kernel.Validation.Create_Cap
                 (Mem_Process, Faulted_Cap,
                  Rose.Capabilities.Layout.Process_Cap);
               Launch_Params.Caps (1) := Faulted_Cap;
            end;

            Send_Cap
              (From_Process => 1,
               To_Process   => Mem_Process,
               Sender_Cap   => 0,
               Receiver_Cap => Mem_Launch_Cap,
               Params       => Launch_Params);
         end if;

         Rose.Kernel.Processes.Queue.Queue_Process (Pid);
      end;

      --  Rose.Kernel.Processes.Queue.Choose_Process;

      return Pid;

   end Load_Boot_Module;

end Rose.Kernel.Processes.Init;
