with Ada.Unchecked_Conversion;

with System;

with Rose.Addresses;                   use Rose.Addresses;
with Rose.Words;                       use Rose.Words;

package Rose.Arch is

   Kernel_CS : constant := 8;
   Kernel_DS : constant := 16;
   User_CS   : constant := 24;   --  for now (or maybe forever)
   User_DS   : constant := 32;   --  same

   type Privilege_Level is mod 2 ** 2;

   function Port_In_8
     (Port  : Word_16)
      return Word_8;

   function Port_In_16
     (Port  : Word_16)
      return Word_16;

   function Port_In_32
     (Port  : Word_16)
      return Word_32;

   procedure Port_Out_8
     (Port  : Word_16;
      Value : Word_8);

   procedure Port_Out_16
     (Port  : Word_16;
      Value : Word_16);

   procedure Port_Out_32
     (Port  : Word_16;
      Value : Word_32);

   function Inb
     (Port  : Word_16)
      return Word_8
      renames Port_In_8;

   procedure Outb
     (Port  : Word_16;
      Value : Word_8)
      renames Port_Out_8;

   procedure IO_Wait is null;   --  because in, out are so lame

   procedure Enable_Interrupts;
   procedure Disable_Interrupts;

   function Has_SSE return Boolean;
   --  True if the processor has SSE and FXSAVE (CPUID).

   procedure Enable_SSE;
   --  Let processes use the FPU and SSE: clear CR0.EM, set CR0.MP and
   --  CR0.NE, set CR4.OSFXSR and CR4.OSXMMEXCPT, and set CR0.TS, so that
   --  a process's first FPU or SSE instruction raises Device_Not_Available
   --  (#170).

   procedure Clear_Task_Switched;
   --  Clear CR0.TS, so that FPU and SSE instructions run.

   procedure Save_FPU_State (Area : System.Address);
   --  FXSAVE to Area, which must be 512 bytes, aligned to 16.

   procedure Restore_FPU_State (Area : System.Address);
   --  FXRSTOR from Area, which must be 512 bytes, aligned to 16.

   procedure Initialize_FPU_State;
   --  A clean FPU and SSE state: FNINIT, and the default MXCSR, with all
   --  SSE exceptions masked.

   procedure Halt;

   procedure Copy_Memory
     (From   : Virtual_Address;
      To     : Virtual_Address;
      Length : Virtual_Bytes);

   function To_Word_32
   is new Ada.Unchecked_Conversion (System.Address, Rose.Words.Word_32);

   type Interrupt_Handler is access procedure;
   pragma Convention (C, Interrupt_Handler);

end Rose.Arch;
