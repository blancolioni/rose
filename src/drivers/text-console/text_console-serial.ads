with Rose.Capabilities;

package Text_Console.Serial is

   --  A copy of what the console writes, on the second serial port,
   --  COM2, when the kernel's command line has serial-console (#904). The
   --  boot tests read it from QEMU's second serial file, where nothing
   --  scrolls away. The kernel's log is on COM1, so the two don't mix.
   --  Escape sequences are left out, so that the copy is plain text.

   procedure Start
     (Command_Line : Rose.Capabilities.Capability;
      Data         : Rose.Capabilities.Capability;
      Status       : Rose.Capabilities.Capability);
   --  Read the boot loader's command line tag through Command_Line, and
   --  copy from now on if it has serial-console, and trace if it has
   --  pointer-trace. Data and Status are
   --  COM2's data port and its line status port. The console calls it
   --  when it starts, and a restorable console again when it is restored,
   --  because each boot has its own command line.

   procedure Put (Text : String);
   --  Copy Text, if Start found serial-console.

   procedure Trace (Text : String);
   --  Write Text and a line feed, if Start found pointer-trace, whether
   --  or not the console is copied: surface-console writes the pointer
   --  events it reads, for scripts/test-screen (#588).

end Text_Console.Serial;
