with Ada.Text_IO;

procedure Rose.Core.List
  (Directory : in out Rose.Interfaces.Files.Directory.Directory_Object'Class)
is
begin
   for Index in 1 .. Directory.Entry_Count loop
      Ada.Text_IO.Put_Line (Directory.Entry_Name (Index));
   end loop;
end Rose.Core.List;
