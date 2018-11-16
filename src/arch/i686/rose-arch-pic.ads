package Rose.Arch.PIC is

   --  We will remap the PICs to the following IRQ base offsets.
   Master_Offset : constant := 16#20#;
   Slave_Offset  : constant := 16#28#;

   PIC_1_Base    : constant := 16#20#;  --  IO base address for master PIC
   PIC_2_Base    : constant := 16#A0#;  --  IO base address for slave PIC

   PIC_1_Command : constant := PIC_1_Base;
   PIC_1_Data    : constant := PIC_1_Base + 1;
   PIC_2_Command : constant := PIC_2_Base;
   PIC_2_Data    : constant := PIC_2_Base + 1;

   PIC_End_Of_Interrupt : constant := 16#20#;

   ICW1_ICW4        : constant := 16#01#;
   ICW1_SINGLE      : constant := 16#02#;
   ICW1_INTERVAL4   : constant := 16#04#;
   ICW1_LEVEL       : constant := 16#08#;
   ICW1_INIT        : constant := 16#10#;

   ICW4_8086        : constant := 16#01#;
   ICW4_AUTO        : constant := 16#02#;
   ICW4_BUF_SLAVE   : constant := 16#08#;
   ICW4_BUF_MASTER  : constant := 16#0C#;
   ICW4_SFNM        : constant := 16#10#;

   procedure Initialise_PIC;
   pragma Export (C, Initialise_PIC, "initialise_pic");

end Rose.Arch.PIC;
