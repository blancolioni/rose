with Login_Loop;

procedure Rose.Core.Login
  (Console       : in out Rose.Interfaces.Screens.Terminal
    .Terminal_Object'Class;
   Timer_Service : in out Rose.Interfaces.Time.Timer.Timer_Object'Class;
   Auth          : in out Rose.Interfaces.Users.Auth.Auth_Object'Class;
   User          : out Rose.Capabilities.Capability)
is
begin
   --  The launch call completes when someone logs in, with their User
   --  interface (#526).

   Login_Loop.Set_Caps
     (Console => Console.Terminal_Cap,
      Timer   => Timer_Service.Timer_Cap,
      Auth    => Auth.Auth_Cap);
   User := Login_Loop.Run;
end Rose.Core.Login;
