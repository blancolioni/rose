with Ada.Text_IO;

procedure Rose.Core.List
  (Directory : Rose.Interfaces.Directory.Client.Directory_Client)
is
   use Rose.Interfaces.Directory.Client;
   Name : String (1 .. 256);
   Last : Natural;
begin
   for Index in 1 .. Directory_Entry_Count (Directory) loop
      Directory_Entry_Name (Directory, Index, Name, Last);
      Ada.Text_IO.Put_Line (Name (Name'First .. Last));
   end loop;
end Rose.Core.List;
