--  with Rose.Invocation;
with Rose.Words;                       use Rose.Words;

with Rose.Boot.Console;

with Rose.Kernel.Processes.Queue;

package body Rose.Kernel.Clock is

   Ticks : Word_32 := 0;

   -----------------------
   -- Handle_Clock_Tick --
   -----------------------

   function Handle_Clock_Tick
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status
   is
      Current_Pid : constant Rose.Objects.Process_Id :=
                      Rose.Kernel.Processes.Current_Process_Id;
   begin
      Ticks := Ticks + 1;

      if Ticks mod 100 = 0 then
         Rose.Boot.Console.Status_Line
           (Current_Pid   => Current_Pid,
            Current_Ticks => Ticks,
            Page_Faults   => Rose.Kernel.Processes.Page_Fault_Count);
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

end Rose.Kernel.Clock;
