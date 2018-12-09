with Rose.Kernel.Interrupts;

package Rose.Kernel.Clock is

   function Handle_Clock_Tick
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status;

   procedure Update_Mem
     (Allocated, Available : Rose.Addresses.Physical_Bytes);

end Rose.Kernel.Clock;
