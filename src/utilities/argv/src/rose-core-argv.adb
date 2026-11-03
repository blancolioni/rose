with Ada.Command_Line;
with Ada.Text_IO;

procedure Rose.Core.Argv
  (Status    : Natural := 0;
   Arguments : Rose.Command_Line.String_List)
is
   Count : constant Natural := Ada.Command_Line.Argument_Count;
   Same  : Boolean := Count = Rose.Command_Line.Length (Arguments);
begin
   Ada.Text_IO.Put_Line
     (Ada.Command_Line.Command_Name & Natural'Image (Count) & " arguments");

   for Index in 1 .. Count loop
      Ada.Text_IO.Put_Line
        (Natural'Image (Index) & ": [" & Ada.Command_Line.Argument (Index)
         & "]");
      if Ada.Command_Line.Argument (Index)
        /= Rose.Command_Line.Element (Arguments, Index)
      then
         Same := False;
      end if;
   end loop;

   Ada.Text_IO.Put_Line
     (if Same then "same as the argument list"
      else "not the same as the argument list");

   Ada.Command_Line.Set_Exit_Status
     (Ada.Command_Line.Exit_Status (Status));
end Rose.Core.Argv;
