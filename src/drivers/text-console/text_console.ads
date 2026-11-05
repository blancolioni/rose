package Text_Console with Pure is

   --  What the two persistent consoles share (ADR 0005, #653): the line
   --  discipline, and the screen as cells of characters. vga-console
   --  draws the cells on the VGA text page, and surface-console on a
   --  surface. Each is a generic, instantiated with the console's own
   --  drawing.

end Text_Console;
