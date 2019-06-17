
package body Rose.Arch.PIC is

   ----------------
   -- Enable_IRQ --
   ----------------

   procedure Enable_IRQ (IRQ : Rose.Words.Word_8) is
      IRQ_Line : constant Word_8 :=
                   (if IRQ < 8 then IRQ else IRQ - 8);
      Port     : constant Word_16 :=
                   (if IRQ < 8 then PIC_1_Data else PIC_2_Data);
      Value    : constant Word_8 :=
                   Inb (Port) and not (2 ** Natural (IRQ_Line));
   begin
      Outb (Port, Value);
   end Enable_IRQ;

   --------------------
   -- Initialise_PIC --
   --------------------

   procedure Initialise_PIC is
      Saved_Mask_1, Saved_Mask_2 : Word_8;
   begin
      Saved_Mask_1 := Inb (PIC_1_Data);
      Saved_Mask_2 := Inb (PIC_2_Data);

      --  start the initialization sequence (in cascade mode)
      Outb (PIC_1_Command, ICW1_INIT + ICW1_ICW4);
      Outb (PIC_2_Command, ICW1_INIT + ICW1_ICW4);

      Outb (PIC_1_Data, Master_Offset);  --  ICW2 : Master PIC Vector Offset
      Outb (PIC_2_Data, Slave_Offset);  -- ICW2: Slave PIC vector offset

      Outb (PIC_1_Data, 4);   --  tell master PIC that there is a slave
                              --  PIC at IRQ 2
      Outb (PIC_2_Data, 2);   --  tell slave PIC its cascade identity

      IO_Wait;

      Outb (PIC_1_Data, ICW4_8086);
      Outb (PIC_2_Data, ICW4_8086);

      --  Restore saved masks
      Outb (PIC_1_Data, Saved_Mask_1);
      Outb (PIC_2_Data, Saved_Mask_2);

   end Initialise_PIC;

end Rose.Arch.PIC;
