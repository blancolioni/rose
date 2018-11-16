with Rose.Kernel.Interrupts;

package Rose.Kernel.Clock is

   function Handle_Clock_Tick
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status;

end Rose.Kernel.Clock;
