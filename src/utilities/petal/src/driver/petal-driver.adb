with Ada.Text_IO;

with Petal.Read;
with Petal.Version;

procedure Petal.Driver is
begin
   Ada.Text_IO.Put_Line
     ("Rose petal, version "
      & Petal.Version.Version_Name);

   Petal.Read.Reader_Loop;

end Petal.Driver;
