with Login_Loop;

procedure Rose.Core.Login
  (Console        : in out Rose.Interfaces.Terminal.Terminal_Object'Class;
   Find_Command   : in out Rose.Interfaces.Commands.Commands_Object'Class;
   Timer_Service  : in out Rose.Interfaces.Timer.Timer_Object'Class;
   Clock_Service  : in out Rose.Interfaces.Clock.Clock_Object'Class;
   Rights_Service : in out Rose.Interfaces.Rights.Rights_Object'Class;
   Auth           : in out Rose.Interfaces.Auth.Auth_Object'Class)
is
begin
   --  The launch call completes, and then the generated main runs
   --  Login_Loop.Run, which never returns.

   Login_Loop.Set_Caps
     (Console  => Console.Terminal_Cap,
      Commands => Find_Command.Commands_Cap,
      Timer    => Timer_Service.Timer_Cap,
      Clock    => Clock_Service.Clock_Cap,
      Rights   => Rights_Service.Rights_Cap,
      Auth     => Auth.Auth_Cap);
end Rose.Core.Login;
