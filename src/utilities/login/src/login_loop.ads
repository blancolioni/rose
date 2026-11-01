with Rose.Capabilities;

package Login_Loop is

   --  Logging users in at the console (#457).

   procedure Set_Caps
     (Console  : Rose.Capabilities.Capability;
      Commands : Rose.Capabilities.Capability;
      Timer    : Rose.Capabilities.Capability;
      Clock    : Rose.Capabilities.Capability;
      Rights   : Rose.Capabilities.Capability;
      Auth     : Rose.Capabilities.Capability);
   --  The interfaces of the console (Terminal), the command registry
   --  (Commands), the timer (Timer), the clock (Clock, which petal gives
   --  to the commands that want it), the rights broker (Rights, which
   --  petal asks for the authority of a command's roles; ADR 0021) and
   --  auth (Auth).

   procedure Run
     with No_Return;
   --  Ask for a user name and password, and run petal for the user until
   --  it logs out; or, if they are wrong, wait and say so. Then ask again.

end Login_Loop;
