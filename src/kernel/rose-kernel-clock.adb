--  with Rose.Invocation;
with Rose.Words;                       use Rose.Words;

with Rose.Boot.Console;
with Rose.Kernel.Heap;
with Rose.Kernel.Processes.Queue;

package body Rose.Kernel.Clock is

   Ticks : Word_32 := 0;

   Current_Allocated_Memory : Physical_Bytes := 0;
   Current_Available_Memory : Physical_Bytes := 0;

   -----------------------
   -- Handle_Clock_Tick --
   -----------------------

   function Handle_Clock_Tick
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status
   is
      Current_Pid : constant Rose.Kernel.Processes.Process_Id :=
                      Rose.Kernel.Processes.Current_Process_Id;
   begin
      Ticks := Ticks + 1;

      if Ticks mod 20 = 0 then
         declare
            Allocated   : Physical_Bytes;
            Available   : Physical_Bytes;
            Name        : String (1 .. 20);
            Name_Last   : Natural;
         begin
            Rose.Kernel.Heap.Get_Status (Allocated, Available);
            Rose.Kernel.Processes.Get_Process_Name
              (Current_Pid, Name, Name_Last);
            Rose.Boot.Console.Status_Line
              (Current_Process => Name (1 .. Name_Last),
               Current_Ticks   => Ticks,
               Page_Faults     => Rose.Kernel.Processes.Page_Fault_Count,
               Mem_Allocated   => Current_Allocated_Memory,
               Mem_Available   => Current_Available_Memory,
               Heap_Allocated  => Allocated,
               Heap_Available  => Available);
         end;
      end if;

      if Rose.Kernel.Processes.Use_Tick then
         Rose.Kernel.Processes.Set_Current_State
           (Current_Pid, Rose.Kernel.Processes.Ready);
         Rose.Kernel.Processes.Queue.Quantum_Finished (Current_Pid);
         return Rose.Kernel.Interrupts.Not_Finished;
      end if;

--      if Ticks mod 10 = 0 then
--           declare
--              use System.Machine_Code;
--              use Rose.Invocation;
--
--              Params : aliased Invocation_Record :=
--                         Invocation_Record'
--                           (Control   =>
--                              (Endpoint => 1,
--                               Flags    => (Send => True, others => False),
--                               others   => <>),
--                            Cap       => 1,
--                            Reply_Cap => 0,
--                            Data      => (others => 0));
--           begin
--              Asm ("movl %0, %%eax",
--                   Inputs   =>
--                     System.Address'Asm_Input
--                       ("g", Params'Address),
--                   Volatile => True);
--              Asm ("int $0x30", Volatile => True);
--           end;
--      end if;
      return Rose.Kernel.Interrupts.Finished;
   end Handle_Clock_Tick;

   ----------------
   -- Update_Mem --
   ----------------

   procedure Update_Mem
     (Allocated, Available : Rose.Addresses.Physical_Bytes)
   is
   begin
      Current_Available_Memory := Available;
      Current_Allocated_Memory := Allocated;
   end Update_Mem;

end Rose.Kernel.Clock;
