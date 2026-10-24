with Ada.Text_IO;

procedure Rose.Core.Echo
  (New_Line  : Boolean := True;
   Arguments : Rose.Command_Line.String_List)
is
begin
   for Index in 1 .. Rose.Command_Line.Length (Arguments) loop
      if Index > 1 then
         Ada.Text_IO.Put (' ');
      end if;
      Ada.Text_IO.Put (Rose.Command_Line.Element (Arguments, Index));
   end loop;

   if New_Line then
      Ada.Text_IO.New_Line;
   else
      Ada.Text_IO.Flush;
   end if;
end Rose.Core.Echo;
