package Tests.Screen is

   function Screen_Test return Boolean;
   --  The screen driver's surface (#582): it has a size, its buffer maps
   --  shared (ADR 0025), and Damage copies what is drawn there to the
   --  screen. It draws a pattern over the whole screen, which stays there
   --  until something else draws: red is the column mod 256, green the
   --  row mod 256, and blue 16#80#. scripts/test-screen looks for it.
   --  Skipped without the screen, as when petal runs it.

end Tests.Screen;
