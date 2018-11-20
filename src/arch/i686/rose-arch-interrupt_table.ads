with Rose.Arch.Interrupts;

package Rose.Arch.Interrupt_Table is

   procedure Create_Boot_Interrupt_Table;
   pragma Export (C, Create_Boot_Interrupt_Table,
                  "create_boot_interrupt_table");

   procedure Generic_Hardware_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      EIP       : Rose.Words.Word_32;
      CS        : Rose.Words.Word_32;
      EFLags    : Rose.Words.Word_32);
   pragma Export (C, Generic_Hardware_Interrupt, "generic_handle_int");

   type Exception_Handler is access procedure;
   pragma Convention (C, Exception_Handler);

   procedure Enable_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector);

end Rose.Arch.Interrupt_Table;
