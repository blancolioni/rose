with Ada.Streams;
with System.Storage_Elements;

with Ada.Text_IO;

with Rose.Interfaces.Objects;
with Rose.Interfaces.Releasable_Reader;
with Rose.Interfaces.Releasable_Writer;

procedure Rose.Core.Copy
  (Source : in out Rose.Interfaces.File.File_Object'Class;
   Target : in out Rose.Interfaces.Transactional_Writable_File
                     .Transactional_Writable_File_Object'Class)
is
   use System.Storage_Elements;
   use type Ada.Streams.Stream_Element_Offset;
   Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
   Last   : Ada.Streams.Stream_Element_Offset;
   Reader : Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class :=
     Source.Open_Reader;
   --  A reader of its own, from the start of the file (ADR 0009).
   Writer : Rose.Interfaces.Releasable_Writer.Releasable_Writer_Object'Class :=
     Target.Rewrite;
   --  The copy replaces the target's contents (ADR 0009, section 4).
begin

   --  Target is a version, which petal releases when the copy ends. It
   --  changes only when it is committed (ADR 0026).

   loop
      Reader.Read (Buffer, Last);
      exit when Last < Buffer'First;
      begin
         Writer.Write (Buffer (Buffer'First .. Last));
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>

            --  A write past the largest file fails, and writes nothing
            --  (#754). An unhandled exception fails the launch call (see
            --  Rose.Command_Line.Launch), before anything is committed.

            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "copy: the target cannot hold more than"
               & Storage_Count'Image (Target.Size) & " of"
               & Storage_Count'Image (Source.Size) & " bytes");
            Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
            raise;
      end;
   end loop;
   Reader.Release;
   Writer.Release;
   Target.Commit;
end Rose.Core.Copy;
