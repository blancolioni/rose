with System.Storage_Elements;

with Ada.Text_IO;

procedure Rose.Core.Cat
  (File : in out Rose.Interfaces.Reader.Reader_Object'Class)
is
   use System.Storage_Elements;
   Buffer : Storage_Array (1 .. 4096);
   Last   : Storage_Count;
begin
   --  File is a reader that petal opened on the named file, or petal's
   --  standard input, which ends at Ctrl-D (ADR 0011). Petal releases a
   --  reader it opened, so cat only reads (#395).

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
