package VGA_Console.Screen is

   --  Rows 1 to 24 of the VGA text screen; row 0 is the kernel's status
   --  line.  Everything written is kept in this process's memory as well
   --  as on the screen, so a checkpoint holds the screen and a restore
   --  can put it back.

   procedure Map;
   --  Map the VGA text page.  The mapping is not part of a checkpoint,
   --  so this is done again after a restore.

   procedure Put (Text : String);
   --  The first Put takes over the screen as the boot console left it.

   procedure Redraw;
   --  After a restore: map the VGA text page again, and put back the
   --  screen and cursor as they were at the checkpoint.

end VGA_Console.Screen;
