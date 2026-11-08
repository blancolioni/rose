with Ada.Text_IO;

procedure Rose.Core.List
  (Directory : in out Rose.Interfaces.Directory.Directory_Object'Class)
is
begin
   for Index in 1 .. Directory.Directory_Entry_Count loop
      Ada.Text_IO.Put_Line (Directory.Directory_Entry_Name (Index));
   end loop;
end Rose.Core.List;
