with Rose.Capabilities;

package Login_Loop is

   --  Logging a user in at the console (#457, #526).

   procedure Set_Caps
     (Console : Rose.Capabilities.Capability;
      Timer   : Rose.Capabilities.Capability;
      Auth    : Rose.Capabilities.Capability);
   --  The interfaces of the console (Terminal), the timer (Timer) and
   --  auth (Auth).

   function Run return Rose.Capabilities.Capability;
   --  Ask for a user name and password until they are right, and return
   --  the user's User interface. For a wrong one, wait and say so, and
   --  ask again.

end Login_Loop;
