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

   procedure Outb (Port  : Word_16;
                   Value : Word_8)
     with Inline_Always;

   function Inb (Port : Word_16)
                return Word_8;

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

   procedure IO_Wait is null;   --  because in, out are so lame

   procedure Enable_Interrupts;
   procedure Disable_Interrupts;

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
