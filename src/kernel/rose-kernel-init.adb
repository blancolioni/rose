with Rose.Boot.Console;

with Rose.Arch;
with Rose.Arch.Interrupts;
with Rose.Arch.Interrupt_Table;

with Rose.Kernel.Clock;
with Rose.Kernel.Command_Line;
with Rose.Kernel.Errors;
with Rose.Kernel.Heap;
with Rose.Kernel.Interrupts;
with Rose.Kernel.Page_Table;
with Rose.Kernel.Physical_Memory;
with Rose.Kernel.Processes.Init;
with Rose.Kernel.Modules.Init;

package body Rose.Kernel.Init is

   function Device_Not_Available
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status;
   --  A process used the FPU or SSE after another process did: switch
   --  the state, and run the instruction again.

   --------------------------
   -- Device_Not_Available --
   --------------------------

   function Device_Not_Available
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status
   is
   begin
      Rose.Kernel.Processes.Switch_FPU_State;
      return Rose.Kernel.Interrupts.Finished;
   end Device_Not_Available;

   Low_RAM_Base  : constant := 16#0000_0000#;
   Low_RAM_Bound : constant := 16#0009_F000#;

   -----------------
   -- Init_Kernel --
   -----------------

   procedure Init_Kernel is
      Page_Table_Size : constant Physical_Bytes :=
        Physical_Page_Bytes --  page directory
        + 64 * Physical_Page_Bytes;   --  initial page tables
      Process_Table_Size : constant Physical_Bytes :=
                             Processes.Process_Table_Heap_Size;
      --  Boot modules, and the processes they launch before the
      --  memory server adds to the heap, take their pages from here:
      --  a page directory and page tables, a stack page, and each
      --  writable segment, including its .bss.  Boot_Module_Pages sets
      --  how many pages they can have (boot-module-pages on the command
      --  line); loading a boot module that needs more is a panic.

      Heap_Size       : Physical_Bytes;
   begin

      Rose.Boot.Console.Put ("Kernel init: ");
      Rose.Boot.Console.Put (Kernel_Physical_Base);
      Rose.Boot.Console.Put (" - ");
      Rose.Boot.Console.Put (Kernel_Physical_Bound);
      Rose.Boot.Console.New_Line;

      --  Protect low RAM and Kernel
      declare
         Success : Boolean;
      begin

         --  Low RAM
         Physical_Memory.Allocate_Region
           (Low_RAM_Base, Low_RAM_Bound,
            Physical_Memory.RAM, Physical_Memory.BIOS,
            Success);
         if not Success then
            Errors.Fatal ("could not allocate low RAM",
                          Low_RAM_Base, Low_RAM_Bound);
         end if;

         --  Kernel

         Physical_Memory.Allocate_Region
           (Kernel_Physical_Base,
            Kernel_Physical_Bound,
            Physical_Memory.RAM,
            Physical_Memory.Initial_System_Image,
            Success);
         if not Success then
            Errors.Fatal ("could not allocate kernel image",
                          Kernel_Physical_Base, Kernel_Physical_Bound);
         end if;
      end;

      Rose.Kernel.Command_Line.Initialise_Command_Line;

      Log_Invocation :=
        Rose.Kernel.Command_Line.Have_Argument ("log-invocation");

      Log_Reply :=
        Rose.Kernel.Command_Line.Have_Argument ("log-reply");

      Log_Port_IO :=
        Rose.Kernel.Command_Line.Have_Argument ("log-port-io");

      Trace_Launch :=
        Rose.Kernel.Command_Line.Have_Argument ("trace-launch");

      if Rose.Kernel.Command_Line.Have_Argument ("trace-object-id") then
         Trace_Object_Id := Rose.Objects.Object_Id
           (Rose.Kernel.Command_Line.Integer_Argument
              ("trace-object-id"));
      end if;

      if Rose.Kernel.Command_Line.Integer_Argument
        ("boot-module-pages") > 0
      then
         Boot_Module_Pages :=
           Rose.Kernel.Command_Line.Integer_Argument ("boot-module-pages");
      end if;

      Heap_Size := Page_Table_Size + Process_Table_Size
        + Physical_Bytes (Boot_Module_Pages) * Physical_Page_Bytes;

      Rose.Boot.Console.Put ("kernel: heap size ");
      Rose.Boot.Console.Put (Natural (Heap_Size / 1024));
      Rose.Boot.Console.Put ("K, with ");
      Rose.Boot.Console.Put (Boot_Module_Pages);
      Rose.Boot.Console.Put (" pages for boot modules");
      Rose.Boot.Console.New_Line;

      Rose.Boot.Console.Put_Line ("Loading boot modules");

      Rose.Kernel.Modules.Init.Load_Modules;

      Rose.Boot.Console.Put_Line ("Initialising heap");

      Rose.Kernel.Heap.Initialise_Heap (Heap_Size);

      Rose.Boot.Console.Put_Line ("Initialising page table");

      Rose.Kernel.Page_Table.Init_Page_Table;

      Rose.Boot.Console.Put_Line ("Initialising process table");

      Rose.Kernel.Processes.Init.Init_Process_Table;

      Rose.Boot.Console.Put_Line ("Starting clock");

      Rose.Kernel.Interrupts.Set_Handler
        (Rose.Arch.Interrupts.Clock_Interrupt,
         Rose.Kernel.Clock.Handle_Clock_Tick'Access);

      --  Processes can use the FPU and SSE; the state of each is switched
      --  when it first uses them after another process has (#170).

      if Rose.Arch.Has_SSE then
         Rose.Kernel.Interrupts.Set_Handler
           (Rose.Arch.Interrupts.Device_Not_Available,
            Device_Not_Available'Access);
         Rose.Arch.Enable_SSE;
      else
         Rose.Boot.Console.Put_Line ("kernel: no SSE");
      end if;

      Rose.Arch.Interrupt_Table.Enable_Interrupt
        (Rose.Arch.Interrupts.Clock_Interrupt);

      if False then
         Physical_Memory.Show_Regions;
      end if;

   end Init_Kernel;

end Rose.Kernel.Init;
