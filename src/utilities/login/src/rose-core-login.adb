with Login_Loop;

procedure Rose.Core.Login
  (Console        : in out Rose.Interfaces.Screens.Terminal
    .Terminal_Object'Class;
   Find_Command   : in out Rose.Interfaces.Commands.Lookup.Lookup_Object'Class;
   Timer_Service  : in out Rose.Interfaces.Time.Timer.Timer_Object'Class;
   Clock_Service  : in out Rose.Interfaces.Time.Clock.Clock_Object'Class;
   Rights_Service : in out Rose.Interfaces.Users.Rights.Rights_Object'Class;
   System_Directory : in out Rose.Interfaces.Files.Fixed_Directory
     .Fixed_Directory_Object'Class;
   Auth           : in out Rose.Interfaces.Users.Auth.Auth_Object'Class)
is
begin
   --  The launch call completes, and then the generated main runs
   --  Login_Loop.Run, which never returns.

   Login_Loop.Set_Caps
     (Console  => Console.Terminal_Cap,
      Commands => Find_Command.Lookup_Cap,
      Timer    => Timer_Service.Timer_Cap,
      Clock    => Clock_Service.Clock_Cap,
      Rights   => Rights_Service.Rights_Cap,
      System   => System_Directory.Fixed_Directory_Cap,
      Auth     => Auth.Auth_Cap);
end Rose.Core.Login;
