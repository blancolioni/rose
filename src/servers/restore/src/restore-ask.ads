--  Asking the person installing a question, before init has a keyboard
--  (#836).  The keyboard driver is one of the installed programs, so it
--  only starts after restore-install has finished.  Until then, this
--  reads the keyboard controller's ports itself, waiting on the timer
--  between reads.  Only restore-install has the caps it needs.

package Restore.Ask is

   function Can_Ask return Boolean;
   --  True if this server has the timer and the keyboard ports, as
   --  restore-install does.

   function Confirm
     (Question : String;
      Answer   : String)
      return Boolean;
   --  Write Question on the console, read a line from the keyboard, and
   --  return True if it is Answer.  The keys understood are the letters,
   --  digits, space, backspace and enter.  False, without asking, if
   --  not Can_Ask.

end Restore.Ask;
