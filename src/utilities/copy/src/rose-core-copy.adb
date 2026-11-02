with System.Storage_Elements;

with Ada.Text_IO;

with Rose.Interfaces.File_Reader;

procedure Rose.Core.Copy
  (Source : in out Rose.Interfaces.File.File_Object'Class;
   Target : in out Rose.Interfaces.Writable_File.Writable_File_Object'Class)
is
   use System.Storage_Elements;
   Buffer : Storage_Array (1 .. 4096);
   Last   : Storage_Count;
   Reader : Rose.Interfaces.File_Reader.File_Reader_Object'Class :=
     Source.Open_Reader;
   --  A reader of its own, from the start of the file (ADR 0009).
begin

   --  Target is a version that petal emptied, and commits only if the
   --  copy completes (#141).

   loop
      Reader.Read (Buffer, Last);
      exit when Last < Buffer'First;
      Target.Write (Buffer (Buffer'First .. Last));
   end loop;
   Reader.Release;

   --  A file drops what is written past its limit (#215), so a copy of
   --  a larger file is short. It fails, and petal discards it.

   if Target.Size /= Source.Size then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "copy: the target holds" & Storage_Count'Image (Target.Size)
         & " of" & Storage_Count'Image (Source.Size) & " bytes");
      Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);

      --  An unhandled exception fails the launch call (see
      --  Rose.Command_Line.Launch).

      raise Constraint_Error;
   end if;
end Rose.Core.Copy;
