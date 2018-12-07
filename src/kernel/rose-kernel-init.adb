with Rose.Boot.Console;

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
      --  for each boot module, five page table pages
      --  page directory, first code and data page, top of stack page
      --  and an environment page
      Boot_Module_Heap   : constant Physical_Bytes :=
                             Physical_Bytes (Modules.Last_Boot_Module)
                             * 16#5000#;

      --  Module_Size : constant Virtual_Bytes :=
      --    Modules.Module_Heap_Size;
      Heap_Size       : constant Physical_Bytes :=
                          Page_Table_Size
                            + Process_Table_Size
                            + Boot_Module_Heap
                            + 16#1_0000#;
      --    Page_Table_Size + Process_Table_Size + Module_Size;

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

      if Rose.Kernel.Command_Line.Have_Argument ("log-object-id") then
         Log_Object_Id := Rose.Objects.Object_Id
           (Rose.Kernel.Command_Line.Integer_Argument
              ("log-object-id"));
      end if;

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

      Rose.Arch.Interrupt_Table.Enable_Interrupt
        (Rose.Arch.Interrupts.Clock_Interrupt);

      if False then
         Physical_Memory.Show_Regions;
      end if;

   end Init_Kernel;

end Rose.Kernel.Init;
