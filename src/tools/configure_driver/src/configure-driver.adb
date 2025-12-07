with Ada.Command_Line;
with Ada.Text_IO;

with Configure.Caps;
with Configure.Reader;
with Configure.Writer;

procedure Configure.Driver is
   use Ada.Command_Line;
   use Ada.Text_IO;
begin
   if Argument_Count /= 2 then
      Put_Line
        (Standard_Error,
         "Usage: " & Ada.Command_Line.Command_Name
         & " driver-name path-to-driver-file");
      Set_Exit_Status (1);
      return;
   end if;

   declare
      Caps : constant Configure.Caps.Cap_Config_List :=
               Configure.Reader.Read (Argument (2));
   begin
      Configure.Writer.Write_Configuration (Argument (1), Caps);
   end;

exception
   when Name_Error =>
      Put_Line (Argument (1) & ": cannot open");
      Set_Exit_Status (2);
end Configure.Driver;
