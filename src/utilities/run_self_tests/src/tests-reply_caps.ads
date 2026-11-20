package Tests.Reply_Caps is

   function Reply_Caps_Test return Boolean;
   --  A reply brings the caller only the caps it asked for (#895). The
   --  test asks its bank's interface cap for a page of four caps but
   --  receives only one, and then for four caps while receiving none.
   --  The kernel gives a new cap the lowest free number, so a cap made
   --  after each call shows whether the call left more caps behind.

end Tests.Reply_Caps;
