with Ada.Streams;

with Rose.Interfaces.Reader;
with Rose.Interfaces.Writer;

package Rose.Streams is

   --  Ada streams over a Reader or a Writer (#727), so that 'Read,
   --  'Write, 'Input and 'Output work directly over a cap:
   --
   --     Input : aliased Reader_Client := ...;
   --     S     : aliased Rose.Streams.Reader_Stream (Input'Access);
   --     ...
   --     Header'Read (S'Access, H);
   --
   --  A Reader_Stream only reads, and a Writer_Stream only writes: the
   --  other operation raises Ada.IO_Exceptions.Mode_Error. The stream does
   --  not release its reader or writer; whoever opened it does.

   type Reader_Stream
     (Reader : not null access Rose.Interfaces.Reader.Reader_Object'Class)
   is new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out Reader_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Fill Item from the reader. A reader can return less than was asked
   --  for, as the console returns a line, so Read goes on reading until
   --  Item is full, or a read returns nothing, which is the end of the
   --  stream. Last is Item'Last, or less at the end of the stream, and
   --  Item'First - 1 if there was nothing to read.

   overriding procedure Write
     (Stream : in out Reader_Stream;
      Item   : Ada.Streams.Stream_Element_Array);
   --  Raise Mode_Error.

   type Writer_Stream
     (Writer : not null access Rose.Interfaces.Writer.Writer_Object'Class)
   is new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out Writer_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Raise Mode_Error.

   overriding procedure Write
     (Stream : in out Writer_Stream;
      Item   : Ada.Streams.Stream_Element_Array);
   --  Write Item to the writer, a page at a time: a write is copied
   --  through the invocation buffer, which is one page.

private

   type Reader_Stream
     (Reader : not null access Rose.Interfaces.Reader.Reader_Object'Class)
   is new Ada.Streams.Root_Stream_Type with null record;

   type Writer_Stream
     (Writer : not null access Rose.Interfaces.Writer.Writer_Object'Class)
   is new Ada.Streams.Root_Stream_Type with null record;

end Rose.Streams;
