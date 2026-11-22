with Ada.Text_IO;

procedure Rose.Core.List
  (Directory : in out Rose.Interfaces.Files.Directory.Directory_Object'Class)
is
begin
   for Index in 1 .. Directory.Entry_Count loop
      declare
         Name : constant String := Directory.Entry_Name (Index);
      begin

         --  A directory and a special file say what they are (ADR 0035,
         --  section 5); an ordinary file is just its name.

         case Directory.Entry_File_Kind (Index) is
            when Rose.Interfaces.Files.Directory.Ordinary_File =>
               Ada.Text_IO.Put_Line (Name);
            when Rose.Interfaces.Files.Directory.Directory =>
               Ada.Text_IO.Put_Line (Name & "  (directory)");
            when Rose.Interfaces.Files.Directory.Special_File =>
               Ada.Text_IO.Put_Line (Name & "  (special file)");
         end case;
      end;
   end loop;
end Rose.Core.List;
