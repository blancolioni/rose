with Rose.Capabilities;

package PS2 is

   pragma Pure (PS2);

   --  The driver of the PS/2 controller (#588): the keyboard on its first
   --  port, and the mouse on its second. One process has to drive both,
   --  because both send their bytes through the same data port.

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Register_IRQ_Cap    : constant Rose.Capabilities.Capability := 3;
   --  The keyboard's interrupt, IRQ 1.
   Data_Cap            : constant Rose.Capabilities.Capability := 4;
   --  Writes the data port, 60.
   Command_Cap         : constant Rose.Capabilities.Capability := 5;
   --  Writes the command port, 64. The keyboard driver had these two the
   --  other way round, and never used them.
   Read_Key_Cap        : constant Rose.Capabilities.Capability := 6;
   --  Reads the data port.
   Read_Status_Cap     : constant Rose.Capabilities.Capability := 7;
   --  Reads the status port, 64.
   Register_Mouse_Cap  : constant Rose.Capabilities.Capability := 8;
   --  The mouse's interrupt, IRQ 12.

end PS2;
