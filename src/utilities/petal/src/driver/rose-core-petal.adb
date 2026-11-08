with Ada.Text_IO;

with Petal.Command_Launch;
with Petal.Read;
with Petal.Session;
with Petal.Version;

procedure Rose.Core.Petal
  (Find_Command   : in out Rose.Interfaces.Commands.Commands_Object'Class;
   Console        : in out Rose.Interfaces.Terminal.Terminal_Object'Class;
   Clock_Service  : in out Rose.Interfaces.Clock.Clock_Object'Class;
   Rights_Service : in out Rose.Interfaces.Rights.Rights_Object'Class;
   User           : in out Rose.Interfaces.User.User_Object'Class)
is
begin
   Standard.Petal.Command_Launch.Set_Registry
     (Commands => Find_Command.Commands_Cap,
      Terminal => Console.Terminal_Cap,
      Clock    => Clock_Service.Clock_Cap,
      Rights   => Rights_Service.Rights_Cap);

   Ada.Text_IO.Put_Line
     ("Rose petal, version "
      & Standard.Petal.Version.Version_Name);

   Standard.Petal.Session.Start (User);
   Standard.Petal.Session.Run_Startup_File;
   Standard.Petal.Read.Reader_Loop;
end Rose.Core.Petal;
