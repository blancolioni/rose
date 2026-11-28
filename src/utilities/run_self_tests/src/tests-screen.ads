package Tests.Screen is

   function Screen_Test return Boolean;

   function Test_Surface_Test return Boolean;
   --  The test surface, memory_surface, which the display server's
   --  tests stand on (#583): launched with Launch, it is 320 by 200, its
   --  buffer maps, an injected event can be read from its events, and
   --  Set_Size gives it a new size and queues a Size event.
   --  The screen driver's surface (#582): it has a size, the pointer is
   --  on it (#588), and its buffer maps (ADR 0025). The console holds
   --  the screen (#653), so the test doesn't draw on it or read its
   --  events: the console's own tests are scripts/test-screen's login,
   --  self tests and mouse steps. Skipped without the screen, as when
   --  petal runs it.

end Tests.Screen;
