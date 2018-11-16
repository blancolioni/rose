with System.Machine_Code;               use System.Machine_Code;

package body Rose.Arch is

   -----------------
   -- Copy_Memory --
   -----------------

   procedure Copy_Memory (From   : Physical_Address;
                          To     : Physical_Address;
                          Length : Physical_Address)
   is
      pragma Unreferenced (From);
      pragma Unreferenced (To);
      pragma Unreferenced (Length);
   begin
      null;
   end Copy_Memory;

   ----------
   -- Halt --
   ----------

   procedure Halt is
   begin
      Asm ("hlt", Volatile => True);
   end Halt;

   ---------
   -- Inb --
   ---------

   function Inb (Port : Word_16)
                return Word_8
   is
      Result : Word_8 := 0;
   begin
      Asm ("pushq %%rdx", Volatile => True);
      Asm ("movw %0, %%dx",
           Inputs => Word_16'Asm_Input ("g", Port),
           Volatile => True);
      Asm ("inb %%dx, %%al", Volatile => True);
      Asm ("movb %%al, %0",
           Outputs => (Word_8'Asm_Output ("=g", Result)),
           Volatile => True);
      Asm ("outb %%al, $0x80", Volatile => True);
      Asm ("popq %%rdx", Volatile => True);
      return Result;
   end Inb;

   ----------
   -- Outb --
   ----------

   procedure Outb (Port  : Word_16;
                   Value : Word_8)
   is
   begin
      Asm ("pushq %%rax", Volatile => True);
      Asm ("pushq %%rdx", Volatile => True);
      Asm ("movb %0, %%al",
           Inputs => (Word_8'Asm_Input ("g", Value)),
           Volatile => True);
      Asm ("movw %0, %%dx",
           Inputs => Word_16'Asm_Input ("g", Port),
           Volatile => True);
      Asm ("outb %%al, %%dx",
           Volatile => True);
      Asm ("outb %%al, $0x80", Volatile => True);
      Asm ("popq %%rdx", Volatile => True);
      Asm ("popq %%rax", Volatile => True);
   end Outb;

end Rose.Arch;
