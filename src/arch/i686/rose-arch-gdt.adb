with Ada.Unchecked_Conversion;
with System;

package body Rose.Arch.GDT is

   type GDT_Pointer_Record is
      record
         Limit : Word_16;
         Base  : Word_32;
      end record;

   for GDT_Pointer_Record use
      record
         Limit at 0 range 0 .. 15;
         Base  at 0 range 16 .. 48;
      end record;

   Global_GDT_Pointer : GDT_Pointer_Record;
   pragma Export (C, Global_GDT_Pointer, "global_gdt_pointer");

   type GDT_Entry is array (0 .. 7) of Word_8
     with Pack, Size => 64;

   type GDT_Entry_Table is array (Word_8) of GDT_Entry
     with Pack, Size => 64 * 256;

   GDTs : GDT_Entry_Table;
   pragma Export (C, GDTs, "global_gdt");

   Interrupt_Stack : array (1 .. 4096) of Word_8;
   pragma Export (C, Interrupt_Stack, "interrupt_stack");

   type Task_State_Segment is
     array (0 .. 25) of Word_32;

   Global_TSS : Task_State_Segment :=
                  (others => 0);
   pragma Export (C, Global_TSS, "global_tss");

   ----------------
   -- Create_GDT --
   ----------------

   procedure Create_GDT is
      function To_Word_32 is
         new Ada.Unchecked_Conversion (System.Address, Rose.Words.Word_32);
   begin
      Global_GDT_Pointer :=
        (Limit => GDTs'Size / 8 - 1,
         Base  => To_Word_32 (GDTs'Address));
      GDTs := (others => (others => 0));
      --  Kernel code and data
      Set_Global_Descriptor
        (1, 0, 16#FFFF_FFFF#, 16#9A#);
      Set_Global_Descriptor
        (2, 0, 16#FFFF_FFFF#, 16#92#);

      --  User code and data
      Set_Global_Descriptor
        (3, 0, 16#FFFF_FFFF#, 16#FA#);
      Set_Global_Descriptor
        (4, 0, 16#FFFF_FFFF#, 16#F2#);

      --  TSS
      Set_Global_Descriptor
        (5,
         To_Word_32 (Global_TSS'Address), Task_State_Segment'Size / 8,
         16#89#);

      Global_TSS (1) := To_Word_32 (Interrupt_Stack'Address) + 4094;
      Global_TSS (2) := 16#10#;
      Global_TSS (25) := 104 * 65536;

   end Create_GDT;

   ---------------------------
   -- Set_Global_Descriptor --
   ---------------------------

   procedure Set_Global_Descriptor
     (Entry_Index     : Rose.Words.Word_8;
      Base, Limit     : Rose.Words.Word_32;
      Descriptor_Type : Rose.Words.Word_8)
   is
      Page_Granularity : constant Boolean :=
                           Limit > 65536;
      Limit_20 : constant Word_32 :=
                   (if Page_Granularity
                    then Limit / 2 ** 12
                    else Limit);
      Target : GDT_Entry renames GDTs (Entry_Index);
   begin
      --  encode the limit
      Target (0) := Word_8 (Limit_20 mod 256);
      Target (1) := Word_8 (Limit_20 / 256 mod 256);
      Target (6) :=
        (if Page_Granularity then 16#C0# else 16#40#)
        + Word_8 (Limit_20 / 65536 mod 16);

      --  encode the base
      Target (2) := Word_8 (Base mod 256);
      Target (3) := Word_8 (Base / 256 mod 256);
      Target (4) := Word_8 (Base / 2 ** 16 mod 256);
      Target (7) := Word_8 (Base / 2 ** 24 mod 256);

      --  encode the type
      Target (5) := Descriptor_Type;

   end Set_Global_Descriptor;

end Rose.Arch.GDT;
