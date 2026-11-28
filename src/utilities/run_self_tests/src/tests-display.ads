package Tests.Display is

   function Display_Test return Boolean;
   --  The display server, trellis (#583), on a test surface,
   --  memory_surface, of 320 by 200, in a bank of the test's own: it
   --  draws the weave on its root; a surface it gives has the root's
   --  size, and its damage reaches the root's buffer, and nothing else
   --  of the root changes; a second surface is refused, until the split;
   --  keys on the root reach the surface, and the pointer's and the
   --  wheel's events don't; and releasing the surface brings the weave
   --  back.

end Tests.Display;
