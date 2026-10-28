with System.Storage_Elements;

with Ada.Text_IO;

procedure Rose.Core.Cat
  (File : in out Rose.Interfaces.File.File_Object'Class)
is
   use System.Storage_Elements;
   Buffer : Storage_Array (1 .. 4096);
   Last   : Storage_Count;
begin
   --  A file has one position, which every reader of it moves, so cat
   --  starts from the beginning.

   File.Seek (0);

   loop
      File.Read (Buffer, Last);
      exit when Last < Buffer'First;

      declare
         Text : String (1 .. Natural (Last));
         for Text'Address use Buffer'Address;
         pragma Import (Ada, Text);
      begin
         Ada.Text_IO.Put (Text);
      end;
   end loop;

   Ada.Text_IO.Flush;
end Rose.Core.Cat;
