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

--     procedure Handle_Interrupt
--       (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector);
--     pragma Export (C, Handle_Interrupt, "handle_interrupt");

   type Exception_Handler is access procedure;
   pragma Convention (C, Exception_Handler);

--     procedure Set_Hardware_Interrupt_Handler
--       (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
--        Handler   : Interrupt_Handler);
--     pragma Export (C, Set_Hardware_Interrupt_Handler, "set_hw_handler");

end Rose.Arch.Interrupt_Table;
