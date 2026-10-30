with Ada.Text_IO;

with Petal.Command_Launch;
with Petal.Read;
with Petal.Session;
with Petal.Version;

procedure Rose.Core.Petal
  (Find_Command : Rose.Interfaces.Commands.Client.Commands_Client;
   Console      : Rose.Interfaces.Terminal.Client.Terminal_Client;
   Clock_Service : Rose.Interfaces.Clock.Client.Clock_Client;
   User         : Rose.Interfaces.User.Client.User_Client)
is
begin
   Standard.Petal.Command_Launch.Set_Registry
     (Commands =>
        Rose.Interfaces.Commands.Client.Get_Interface_Cap (Find_Command),
      Terminal =>
        Rose.Interfaces.Terminal.Client.Get_Interface_Cap (Console),
      Clock    =>
        Rose.Interfaces.Clock.Client.Get_Interface_Cap (Clock_Service));

   Ada.Text_IO.Put_Line
     ("Rose petal, version "
      & Standard.Petal.Version.Version_Name);

   Standard.Petal.Session.Start (User);
   Standard.Petal.Session.Run_Startup_File;
   Standard.Petal.Read.Reader_Loop;
end Rose.Core.Petal;
