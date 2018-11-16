with Rose.Words;

with Rose.Arch.Interrupts;

package Rose.Kernel.Interrupts is

   type Interrupt_Handler_Status is (Finished, Not_Finished);

   type Interrupt_Handler is access function return Interrupt_Handler_Status;

   type Interrupt_Word_Handler is access
     function (Argument : Rose.Words.Word)
     return Interrupt_Handler_Status;

   type Exception_Handler is access procedure;

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Interrupt_Handler);

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Interrupt_Word_Handler);

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Exception_Handler);

   function Handle_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Argument  : Rose.Words.Word)
      return Interrupt_Handler_Status;

end Rose.Kernel.Interrupts;
