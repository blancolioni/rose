with System.Machine_Code;               use System.Machine_Code;
with System.Storage_Elements;

package body Rose.Arch is

   -------------------------
   -- Clear_Task_Switched --
   -------------------------

   procedure Clear_Task_Switched is
   begin
      Asm ("clts", Volatile => True);
   end Clear_Task_Switched;

   -----------------
   -- Copy_Memory --
   -----------------

   procedure Copy_Memory
     (From   : Virtual_Address;
      To     : Virtual_Address;
      Length : Virtual_Bytes)
   is
      Src_Addr : constant System.Address :=
                   To_System_Address (From);
      Dst_Addr : constant System.Address :=
                   To_System_Address (To);
      Count    : constant System.Storage_Elements.Storage_Count :=
                   System.Storage_Elements.Storage_Count (Length);
      Src      : System.Storage_Elements.Storage_Array (1 .. Count);
      pragma Import (Ada, Src);
      for Src'Address use Src_Addr;
      Dst      : System.Storage_Elements.Storage_Array (1 .. Count);
      pragma Import (Ada, Dst);
      for Dst'Address use Dst_Addr;
   begin
      Dst := Src;
   end Copy_Memory;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      Asm ("cli", Volatile => True);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      Asm ("sti", Volatile => True);
   end Enable_Interrupts;

   ----------------
   -- Enable_SSE --
   ----------------

   procedure Enable_SSE is
      CR0_EM         : constant Word_32 := 16#0004#;
      CR0_MP         : constant Word_32 := 16#0002#;
      CR0_NE         : constant Word_32 := 16#0020#;
      CR0_TS         : constant Word_32 := 16#0008#;
      CR4_OSFXSR     : constant Word_32 := 16#0200#;
      CR4_OSXMMEXCPT : constant Word_32 := 16#0400#;
      CR0            : Word_32;
      CR4            : Word_32;
   begin
      Asm ("movl %%cr0, %0",
           Outputs  => Word_32'Asm_Output ("=r", CR0),
           Volatile => True);
      CR0 := (CR0 and not CR0_EM) or CR0_MP or CR0_NE;
      Asm ("movl %0, %%cr0",
           Inputs   => Word_32'Asm_Input ("r", CR0),
           Volatile => True);

      Asm ("movl %%cr4, %0",
           Outputs  => Word_32'Asm_Output ("=r", CR4),
           Volatile => True);
      CR4 := CR4 or CR4_OSFXSR or CR4_OSXMMEXCPT;
      Asm ("movl %0, %%cr4",
           Inputs   => Word_32'Asm_Input ("r", CR4),
           Volatile => True);

      Initialize_FPU_State;

      Asm ("movl %0, %%cr0",
           Inputs   => Word_32'Asm_Input ("r", CR0 or CR0_TS),
           Volatile => True);
   end Enable_SSE;

   ----------
   -- Halt --
   ----------

   procedure Halt is
   begin
      Asm ("hlt", Volatile => True);
   end Halt;

   -------------
   -- Has_SSE --
   -------------

   function Has_SSE return Boolean is
      CPUID_FXSR : constant Word_32 := 2 ** 24;
      CPUID_SSE  : constant Word_32 := 2 ** 25;
      Features   : Word_32;
   begin
      Asm ("movl $1, %%eax" & ASCII.LF & ASCII.HT
           & "cpuid",
           Outputs  => Word_32'Asm_Output ("=d", Features),
           Clobber  => "eax, ebx, ecx",
           Volatile => True);
      return (Features and CPUID_FXSR) /= 0
        and then (Features and CPUID_SSE) /= 0;
   end Has_SSE;

   --------------------------
   -- Initialize_FPU_State --
   --------------------------

   procedure Initialize_FPU_State is
      MXCSR : constant Word_32 := 16#1F80#;
   begin
      Asm ("fninit", Volatile => True);
      Asm ("ldmxcsr %0",
           Inputs   => Word_32'Asm_Input ("m", MXCSR),
           Volatile => True);
   end Initialize_FPU_State;

   ---------------
   -- Port_In_8 --
   ---------------

   function Port_In_8
     (Port  : Word_16)
      return Word_8
   is
      Result : Word_8;
   begin
      Asm ("movw %0, %%dx",
           Inputs   => Word_16'Asm_Input ("g", Port),
           Volatile => True);
      Asm ("inb %%dx, %%al", Volatile => True);
      Asm ("movb %%al, %0",
           Outputs  => (Word_8'Asm_Output ("=g", Result)),
           Volatile => True);
      return Result;
   end Port_In_8;

   ----------------
   -- Port_In_16 --
   ----------------

   function Port_In_16
     (Port  : Word_16)
      return Word_16
   is
      Result : Word_16;
   begin
      Asm ("movw %0, %%dx",
           Inputs   => Word_16'Asm_Input ("g", Port),
           Volatile => True);
      Asm ("inw %%dx, %%ax", Volatile => True);
      Asm ("movw %%ax, %0",
           Outputs  => (Word_16'Asm_Output ("=g", Result)),
           Volatile => True);
      return Result;
   end Port_In_16;

   ----------------
   -- Port_In_32 --
   ----------------

   function Port_In_32
     (Port  : Word_16)
      return Word_32
   is
      Result : Word_32;
   begin
      Asm ("movw %0, %%dx",
           Inputs   => Word_16'Asm_Input ("g", Port),
           Volatile => True);
      Asm ("inl %%dx, %%eax", Volatile => True);
      Asm ("movl %%eax, %0",
           Outputs  => (Word_32'Asm_Output ("=g", Result)),
           Volatile => True);
      return Result;
   end Port_In_32;

   ----------------
   -- Port_Out_8 --
   ----------------

   procedure Port_Out_8
     (Port  : Word_16;
      Value : Word_8)
   is
   begin
      Asm ("outb %0, %1",
           Inputs   => (Word_8'Asm_Input ("a", Value),
                        Word_16'Asm_Input ("d", Port)),
           Volatile => True);
   end Port_Out_8;

   -----------------
   -- Port_Out_16 --
   -----------------

   procedure Port_Out_16
     (Port  : Word_16;
      Value : Word_16)
   is
   begin
      Asm ("outw %0, %1",
           Inputs   => (Word_16'Asm_Input ("a", Value),
                        Word_16'Asm_Input ("d", Port)),
           Volatile => True);
   end Port_Out_16;

   -----------------
   -- Port_Out_32 --
   -----------------

   procedure Port_Out_32
     (Port  : Word_16;
      Value : Word_32)
   is
   begin
      Asm ("outl %0, %1",
           Inputs   => (Word_32'Asm_Input ("a", Value),
                        Word_16'Asm_Input ("d", Port)),
           Volatile => True);
   end Port_Out_32;

   -------------
   -- Restart --
   -------------

   procedure Restart is
      Keyboard_Status  : constant Word_16 := 16#64#;
      Keyboard_Command : constant Word_16 := 16#64#;
      Input_Full       : constant Word_8 := 16#02#;
      Pulse_Reset      : constant Word_8 := 16#FE#;
      Reset_Control    : constant Word_16 := 16#CF9#;
      Descriptor       : array (1 .. 3) of Word_16 := (0, 0, 0)
        with Volatile;
      --  An interrupt descriptor table with a limit of zero: any interrupt
      --  then faults, the fault faults, and the processor resets.
   begin
      Disable_Interrupts;

      --  The keyboard controller pulses the reset line once its input
      --  buffer is empty.

      for I in 1 .. 100_000 loop
         exit when (Port_In_8 (Keyboard_Status) and Input_Full) = 0;
      end loop;
      Port_Out_8 (Keyboard_Command, Pulse_Reset);

      for I in 1 .. 100_000 loop
         exit when (Port_In_8 (Keyboard_Status) and Input_Full) = 0;
      end loop;

      --  A hard reset through the reset control register.

      Port_Out_8 (Reset_Control, 16#02#);
      Port_Out_8 (Reset_Control, 16#06#);

      --  A triple fault.

      Asm ("lidt (%0)" & ASCII.LF & ASCII.HT & "int3",
           Inputs   => System.Address'Asm_Input ("r", Descriptor'Address),
           Volatile => True);

      loop
         Halt;
      end loop;
   end Restart;

   -----------------------
   -- Restore_FPU_State --
   -----------------------

   procedure Restore_FPU_State (Area : System.Address) is
   begin
      Asm ("fxrstor (%0)",
           Inputs   => System.Address'Asm_Input ("r", Area),
           Clobber  => "memory",
           Volatile => True);
   end Restore_FPU_State;

   --------------------
   -- Save_FPU_State --
   --------------------

   procedure Save_FPU_State (Area : System.Address) is
   begin
      Asm ("fxsave (%0)",
           Inputs   => System.Address'Asm_Input ("r", Area),
           Clobber  => "memory",
           Volatile => True);
   end Save_FPU_State;

end Rose.Arch;
