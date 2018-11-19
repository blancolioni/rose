with Rose.Boot.Console;

package body Rose.Kernel.Arch is

   procedure Show_Iret_Stack
     (EIP, CS, PSW, ESP, SS : Rose.Words.Word_32);
   pragma Export (C, Show_Iret_Stack, "show_iret_stack");

   procedure Show_Irq_Stack
     (IRQ, EIP, CS, PSW, ESP, SS : Rose.Words.Word_32);
   pragma Export (C, Show_Irq_Stack, "show_irq_stack");

   procedure Show_Saved_Registers
     (GSFS, ESDS, EDI, ESI : Rose.Words.Word_32;
      EBP, ESP             : Rose.Words.Word_32;
      EAX, ECX, EDX, EBX   : Rose.Words.Word_32);
   pragma Export (C, Show_Saved_Registers, "show_saved_registers");

   -------------------------
   -- Process_Stack_Frame --
   -------------------------

   function Process_Stack_Frame
     (Start_EIP       : Word_32;
      Start_ESP       : Word_32 := Process_Stack_Bound;
      Start_PSW       : Word_32 := 16#0000_0200#;
      Code_Segment    : Word_32 := 16#18#;
      Data_Segment    : Word_32 := 16#20#;
      Privilege_Level : Word_32 := 3)
      return Stack_Frame
   is
   begin
      return Stack_Frame'
        (GS  => Word_16 (Data_Segment or Privilege_Level),
         FS  => Word_16 (Data_Segment or Privilege_Level),
         ES  => Word_16 (Data_Segment or Privilege_Level),
         DS  => Word_16 (Data_Segment or Privilege_Level),
         EDI => 0,
         ESI => 0,
         EBP => 0,
         XSP => Start_ESP,
         EBX => 0,
         EDX => 0,
         ECX => 0,
         EAX => 0,
         EIP => Start_EIP,
         CS  => Code_Segment or Privilege_Level,
         PSW => Start_PSW,
         ESP => Start_ESP,
         SS  => Data_Segment or Privilege_Level);
   end Process_Stack_Frame;

   ---------------------
   -- Show_Iret_Stack --
   ---------------------

   procedure Show_Iret_Stack
     (EIP, CS, PSW, ESP, SS : Rose.Words.Word_32)
   is
      use Rose.Boot.Console;
   begin
      Put ("EIP ");
      Put (EIP);
      Put (" CS ");
      Put (CS);
      Put (" PSW ");
      Put (PSW);
      Put (" ESP ");
      Put (ESP);
      Put (" SS ");
      Put (SS);
      New_Line;
   end Show_Iret_Stack;

   --------------------
   -- Show_Irq_Stack --
   --------------------

   procedure Show_Irq_Stack
     (IRQ, EIP, CS, PSW, ESP, SS : Rose.Words.Word_32)
   is
      use Rose.Boot.Console;
   begin
      Put ("IRQ ");
      Put (IRQ);
      Put (" EIP ");
      Put (EIP);
      Put (" CS ");
      Put (CS);
      Put (" PSW ");
      Put (PSW);
      Put (" ESP ");
      Put (ESP);
      Put (" SS ");
      Put (SS);
      New_Line;
   end Show_Irq_Stack;

   --------------------------
   -- Show_Saved_Registers --
   --------------------------

   procedure Show_Saved_Registers
     (GSFS, ESDS, EDI, ESI : Rose.Words.Word_32;
      EBP, ESP             : Rose.Words.Word_32;
      EAX, ECX, EDX, EBX   : Rose.Words.Word_32)
   is
      use Rose.Boot.Console;
   begin
      Put ("GSFS ");
      Put (GSFS);
      Put (" ESDS ");
      Put (ESDS);
      Put (" EDI ");
      Put (EDI);
      Put (" ESI ");
      Put (ESI);
      New_Line;
      Put (" EBP ");
      Put (EBP);
      Put (" ESP ");
      Put (ESP);
      New_Line;
      Put ("EAX ");
      Put (EAX);
      Put (" ECX ");
      Put (ECX);
      Put (" EDX ");
      Put (EDX);
      Put (" EBX ");
      Put (EBX);
      New_Line;
   end Show_Saved_Registers;

end Rose.Kernel.Arch;
