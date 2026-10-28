with System.Storage_Elements;

with Ada.Text_IO;

with Rose.Interfaces.File_Reader;

procedure Rose.Core.Cat
  (File : in out Rose.Interfaces.File.File_Object'Class)
is
   use System.Storage_Elements;
   Buffer : Storage_Array (1 .. 4096);
   Last   : Storage_Count;
   Reader : Rose.Interfaces.File_Reader.File_Reader_Object'Class :=
     File.Open_Reader;
   --  A reader of its own, from the start of the file (ADR 0009).
begin
   loop
      Reader.Read (Buffer, Last);
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
   Reader.Release;
end Rose.Core.Cat;
