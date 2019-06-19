with Rose.Words;                       use Rose.Words;

with Rose.Arch.PIC;

with Rose.Boot.Console;

with Rose.Kernel.Interrupts;

with Rose.Kernel.Processes.Queue;

package body Rose.Arch.Interrupt_Table is

   type IDRT_Record is
      record
         Limit : Word_16;
         Base  : Word_32;
      end record
     with Pack, Size => 48;

   Global_Irq_Pointer : IDRT_Record;
   pragma Export (C, Global_Irq_Pointer, "global_irq_pointer");

   type IDRT_Entry is new Word_64;

   type IDRT_Entry_Table is array (Word_8) of IDRT_Entry
     with Pack, Size => 64 * 256;

   IDRT_Table : IDRT_Entry_Table;
   pragma Export (C, IDRT_Table, "global_idrt_table");

   procedure Set_Handler
     (IRQ       : Word_8;
      Handler   : Exception_Handler;
      Selector  : Word_16;
      Type_Attr : Word_8);
   pragma Export (C, Set_Handler, "set_irq_handler");

   IRQ_Active : array (Rose.Arch.Interrupts.Interrupt_Vector) of Word_8;
   pragma Export (C, IRQ_Active, "irq_active");

   procedure Divide_Exception;
   pragma Import (C, Divide_Exception, "divide_exception");

   procedure Debug_Exception;
   pragma Import (C, Debug_Exception, "debug_exception");

   procedure NMI_Exception;
   pragma Import (C, NMI_Exception, "nmi_exception");

   procedure Breakpoint_Exception;
   pragma Import (C, Breakpoint_Exception, "breakpoint_exception");

   procedure Overflow_Exception;
   pragma Import (C, Overflow_Exception, "overflow_exception");

   procedure Bound_Exception;
   pragma Import (C, Bound_Exception, "bound_exception");

   procedure Invalid_Opcode_Exception;
   pragma Import (C, Invalid_Opcode_Exception, "invalid_opcode_exception");

   procedure No_Device_Exception;
   pragma Import (C, No_Device_Exception, "no_device_exception");

   procedure Double_Fault_Exception;
   pragma Import (C, Double_Fault_Exception, "double_fault_exception");

   procedure Invalid_Tss_Exception;
   pragma Import (C, Invalid_Tss_Exception, "invalid_tss_exception");

   procedure Segment_Not_Present_Exception;
   pragma Import (C, Segment_Not_Present_Exception,
                  "segment_not_present_exception");

   procedure Stack_Fault_Exception;
   pragma Import (C, Stack_Fault_Exception, "stack_fault_exception");

   procedure Protection_Fault_Exception;
   pragma Import (C, Protection_Fault_Exception, "protection_fault_exception");

   procedure Page_Fault_Exception;
   pragma Import (C, Page_Fault_Exception, "page_fault_exception");

   procedure IRQ_00;
   pragma Import (C, IRQ_00, "hwint0");

   procedure IRQ_01;
   pragma Import (C, IRQ_01, "hwint1");

   procedure IRQ_02;
   pragma Import (C, IRQ_02, "hwint2");

   procedure IRQ_03;
   pragma Import (C, IRQ_03, "hwint3");

   procedure IRQ_04;
   pragma Import (C, IRQ_04, "hwint4");

   procedure IRQ_05;
   pragma Import (C, IRQ_05, "hwint5");

   procedure IRQ_06;
   pragma Import (C, IRQ_06, "hwint6");

   procedure IRQ_07;
   pragma Import (C, IRQ_07, "hwint7");

   procedure IRQ_08;
   pragma Import (C, IRQ_08, "hwint8");

   procedure IRQ_09;
   pragma Import (C, IRQ_09, "hwint9");

   procedure IRQ_10;
   pragma Import (C, IRQ_10, "hwint10");

   procedure IRQ_11;
   pragma Import (C, IRQ_11, "hwint11");

   procedure IRQ_12;
   pragma Import (C, IRQ_12, "hwint12");

   procedure IRQ_13;
   pragma Import (C, IRQ_13, "hwint13");

   procedure IRQ_14;
   pragma Import (C, IRQ_14, "hwint14");

   procedure IRQ_15;
   pragma Import (C, IRQ_15, "hwint15");

   procedure Handle_Processor_Exception
     (Vector : Rose.Arch.Interrupts.Interrupt_Vector;
      Code   : Rose.Words.Word_32);
   pragma Export (C, Handle_Processor_Exception, "exception_handler");

   procedure Handle_Hardware_Interrupt
     (Vector : Rose.Arch.Interrupts.Interrupt_Vector);
   pragma Export (C, Handle_Hardware_Interrupt, "handle_interrupt");

   procedure Set_Exception_Handler
     (Vector : Rose.Arch.Interrupts.Interrupt_Vector;
      Proc   : Exception_Handler);

   ---------------------------------
   -- Create_Boot_Interrupt_Table --
   ---------------------------------

   procedure Create_Boot_Interrupt_Table is
      use Rose.Arch.Interrupts;
      procedure Handler
        (Vector : Rose.Arch.Interrupts.Interrupt_Vector;
         Proc   : Exception_Handler)
         renames Set_Exception_Handler;

   begin
      Global_Irq_Pointer.Limit := IDRT_Table'Size / 8 - 1;

      Handler (Divide_By_Zero, Divide_Exception'Access);
      Handler (Debug, Debug_Exception'Access);
      Handler (Non_Maskable_Interrupt, NMI_Exception'Access);
      Handler (Breakpoint, Breakpoint_Exception'Access);
      Handler (Overflow, Overflow_Exception'Access);
      Handler (Bound_Range_Exceeded, Bound_Exception'Access);
      Handler (Invalid_Opcode, Invalid_Opcode_Exception'Access);
      Handler (Device_Not_Available, No_Device_Exception'Access);
      Handler (Double_Fault, Double_Fault_Exception'Access);
      Handler (Invalid_TSS, Invalid_Tss_Exception'Access);
      Handler (Segment_Not_Present, Segment_Not_Present_Exception'Access);
      Handler (Stack_Segment_Fault, Stack_Fault_Exception'Access);
      Handler (General_Protection_Fault,
               Protection_Fault_Exception'Access);
      Handler (Page_Fault, Page_Fault_Exception'Access);

      Handler (Clock_Interrupt, IRQ_00'Access);
      Handler (Keyboard_Interrupt, IRQ_01'Access);
      Handler (34, IRQ_02'Access);
      Handler (35, IRQ_03'Access);
      Handler (36, IRQ_04'Access);
      Handler (37, IRQ_05'Access);
      Handler (38, IRQ_06'Access);
      Handler (39, IRQ_07'Access);
      Handler (40, IRQ_08'Access);
      Handler (41, IRQ_09'Access);
      Handler (42, IRQ_10'Access);
      Handler (43, IRQ_11'Access);
      Handler (44, IRQ_12'Access);
      Handler (45, IRQ_13'Access);
      Handler (46, IRQ_14'Access);
      Handler (47, IRQ_15'Access);

   end Create_Boot_Interrupt_Table;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector)
   is
   begin
      if Interrupt in 32 .. 47 then
         Rose.Arch.PIC.Enable_IRQ (Rose.Words.Word_8 (Interrupt) - 32);
      else
         Rose.Boot.Console.Put ("enable-interrupt: invalid interrupt: ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Interrupt));
         Rose.Boot.Console.New_Line;
      end if;
   end Enable_Interrupt;

   --------------------------------
   -- Generic_Hardware_Interrupt --
   --------------------------------

   procedure Generic_Hardware_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      EIP       : Rose.Words.Word_32;
      CS        : Rose.Words.Word_32;
      EFLags    : Rose.Words.Word_32)
   is
      pragma Unreferenced (Interrupt, CS, EFLags);
   begin
      Rose.Boot.Console.New_Line;
      Rose.Boot.Console.Put ("interrupt at ");
      Rose.Boot.Console.Put (EIP);
      Rose.Boot.Console.New_Line;
   end Generic_Hardware_Interrupt;

   -------------------------------
   -- Handle_Hardware_Interrupt --
   -------------------------------

   procedure Handle_Hardware_Interrupt
     (Vector : Rose.Arch.Interrupts.Interrupt_Vector)
   is
   begin
      Handle_Processor_Exception (Vector, 0);
   end Handle_Hardware_Interrupt;

   --------------------------------
   -- Handle_Processor_Exception --
   --------------------------------

   procedure Handle_Processor_Exception
     (Vector : Rose.Arch.Interrupts.Interrupt_Vector;
      Code   : Rose.Words.Word_32)
   is
      Interrupted_Process_Id : constant Rose.Kernel.Processes.Process_Id :=
                                 Rose.Kernel.Processes.Current_Process_Id;
   begin
      Rose.Kernel.Processes.Set_Current_State
        (Interrupted_Process_Id,
         Rose.Kernel.Processes.Interrupted);

      declare
         use Rose.Kernel.Interrupts;
         Status : constant Interrupt_Handler_Status :=
                    Rose.Kernel.Interrupts.Handle_Interrupt
                      (Vector, Code);
      begin
         case Status is
            when Finished =>
               IRQ_Active (Vector) := 0;
               Rose.Kernel.Processes.Set_Current_State
                 (Interrupted_Process_Id, Rose.Kernel.Processes.Ready);
               Rose.Kernel.Processes.Queue.Resume_Current_Process;
            when Not_Finished =>
               IRQ_Active (Vector) := 1;
               Rose.Kernel.Processes.Queue.Choose_Process;
         end case;
      end;

   end Handle_Processor_Exception;

   ---------------------------
   -- Set_Exception_Handler --
   ---------------------------

   procedure Set_Exception_Handler
     (Vector : Rose.Arch.Interrupts.Interrupt_Vector;
      Proc   : Exception_Handler)
   is
   begin
      Set_Handler (Word_8 (Vector), Proc,
                   16#08#, 16#8E#);
   end Set_Exception_Handler;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (IRQ       : Word_8;
      Handler   : Exception_Handler;
      Selector  : Word_16;
      Type_Attr : Word_8)
   is
      function To_Word_32 is
        new Ada.Unchecked_Conversion (Exception_Handler, Word_32);
      H : constant Word_32 := To_Word_32 (Handler);
   begin
      IDRT_Table (IRQ) :=
        IDRT_Entry (H / 2 ** 16) * 2 ** 48
        + IDRT_Entry (Type_Attr) * 2 ** 40
        + IDRT_Entry (Selector) * 2 ** 16
        + IDRT_Entry (H mod 2 ** 16) * 2 ** 0;
   end Set_Handler;

   ------------------------------
   -- Start_Interrupt_Handling --
   ------------------------------

--     procedure Start_Interrupt_Handling is
--     begin
--        if True then
--           Rose.Arch.Outb (16#21#, 16#00#);
--           Rose.Arch.Outb (16#A1#, 16#FF#);
--        end if;
--     end Start_Interrupt_Handling;

end Rose.Arch.Interrupt_Table;
