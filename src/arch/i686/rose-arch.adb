with System.Machine_Code;               use System.Machine_Code;
with System.Storage_Elements;

package body Rose.Arch is

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
      Asm ("pushl %%edx", Volatile => True);
      Asm ("movw %0, %%dx",
           Inputs => Word_16'Asm_Input ("g", Port),
           Volatile => True);
      Asm ("inb %%dx, %%al", Volatile => True);
      Asm ("movb %%al, %0",
           Outputs => (Word_8'Asm_Output ("=g", Result)),
           Volatile => True);
      Asm ("outb %%al, $0x80", Volatile => True);
      Asm ("popl %%edx", Volatile => True);
      return Result;
   end Inb;

   ----------
   -- Outb --
   ----------

   procedure Outb (Port  : Word_16;
                   Value : Word_8)
   is
   begin
      Asm ("outb %0, %1",
           Inputs => (Word_8'Asm_Input ("a", Value),
                      Word_16'Asm_Input ("d", Port)),
           Volatile => True);
   end Outb;

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

end Rose.Arch;
