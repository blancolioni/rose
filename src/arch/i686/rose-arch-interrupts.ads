package Rose.Arch.Interrupts is

   type Interrupt_Vector is range 0 .. 63;

   Divide_By_Zero                : constant Interrupt_Vector := 0;
   Debug                         : constant Interrupt_Vector := 1;
   Non_Maskable_Interrupt        : constant Interrupt_Vector := 2;
   Breakpoint                    : constant Interrupt_Vector := 3;
   Overflow                      : constant Interrupt_Vector := 4;
   Bound_Range_Exceeded          : constant Interrupt_Vector := 5;
   Invalid_Opcode                : constant Interrupt_Vector := 6;
   Device_Not_Available          : constant Interrupt_Vector := 7;
   Double_Fault                  : constant Interrupt_Vector := 8;
   Invalid_TSS                   : constant Interrupt_Vector := 10;
   Segment_Not_Present           : constant Interrupt_Vector := 11;
   Stack_Segment_Fault           : constant Interrupt_Vector := 12;
   General_Protection_Fault      : constant Interrupt_Vector := 13;
   Page_Fault                    : constant Interrupt_Vector := 14;
   Floating_Point_Exception_X87  : constant Interrupt_Vector := 16;
   Alignment_Check               : constant Interrupt_Vector := 17;
   Machine_Check                 : constant Interrupt_Vector := 18;
   Floating_Point_Exception_SIMD : constant Interrupt_Vector := 19;
   Virtualization_Exception      : constant Interrupt_Vector := 20;
   Security_Exception            : constant Interrupt_Vector := 30;

   Clock_Interrupt    : constant Interrupt_Vector := 32;
   Keyboard_Interrupt : constant Interrupt_Vector := 33;

end Rose.Arch.Interrupts;
