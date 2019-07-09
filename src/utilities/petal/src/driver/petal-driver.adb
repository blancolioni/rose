with Ada.Text_IO;

with Petal.Commands.Echo;

with Petal.Exec;
with Petal.Read;
with Petal.Version;

procedure Petal.Driver is
begin
   Petal.Exec.Install
     ("echo", Petal.Commands.Echo.Echo_Command);

   Ada.Text_IO.Put_Line
     ("Rose petal, version "
      & Petal.Version.Version_Name);

   Petal.Read.Reader_Loop;

end Petal.Driver;
