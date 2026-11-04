package Tests.Screen is

   function Screen_Test return Boolean;
   --  The screen driver's surface (#582): it has a size and no pointer,
   --  and its buffer maps (ADR 0025). The console holds the screen
   --  (#653), so the test doesn't draw on it or read its events: the
   --  console's own tests are scripts/test-screen's login and self
   --  tests. Skipped without the screen, as when petal runs it.

end Tests.Screen;
