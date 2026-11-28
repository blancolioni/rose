with System.Storage_Elements;

with Rose.Objects;
with Rose.Interfaces.Streams.Reader;
with Rose.Interfaces.Streams.Writer;

generic
   type Element is private;
   Type_Id : Rose.Objects.Endpoint_Id;
   with function Fields return System.Storage_Elements.Storage_Array;
   with function Encoded_Length
     (Item : Element)
      return System.Storage_Elements.Storage_Count;
   with procedure Encode
     (Item   : Element;
      Buffer : in out System.Storage_Elements.Storage_Array;
      Last   : in out System.Storage_Elements.Storage_Offset);
   with procedure Decode
     (Buffer : System.Storage_Elements.Storage_Array;
      Last   : in out System.Storage_Elements.Storage_Offset;
      Item   : out Element);
package Rose.Typed_Streams is

   --  A typed stream (ADR 0013, ADR 0048 section 7, #140): a byte stream
   --  whose bytes are a list of one record type. Instantiate it with a
   --  record's type, type id, field table and codec, which idle generates
   --  in the record's interface package:
   --
   --    package File_Info_Streams is
   --      new Rose.Typed_Streams
   --        (Element        => Core.Files.File_Info,
   --         Type_Id        => Core.Files.File_Info_Type_Id,
   --         Fields         => Core.Files.File_Info_Fields,
   --         Encoded_Length => Core.Files.Encoded_Length,
   --         Encode         => Core.Files.Encode,
   --         Decode         => Core.Files.Decode);
   --
   --  A list is a header and the records' encodings, one after another:
   --
   --    a word, 16#3153_4C52#, "RLS1" in its bytes
   --    the type id, two words, low first
   --    the length of the field table in bytes, a word
   --    the field table
   --    each record, as its codec encodes it
   --
   --  Words are little-endian. The stream's end is the list's end.

   Format_Error : exception;
   --  The stream isn't a list of Element: it has no header, a list of
   --  another record, or a record that is cut short or doesn't decode.

   Default_Capacity : constant := 4096;

   type Writer
     (Target   : not null access
        Rose.Interfaces.Streams.Writer.Writer_Object'Class;
      Capacity : System.Storage_Elements.Storage_Count)
   is tagged limited private;
   --  Writes a list to Target. Records are kept until Capacity bytes of
   --  them would be, and each Write sends whole records. A record longer
   --  than Capacity is sent by itself. Capacity is at least 16, the
   --  length of the header without its table.

   procedure Put
     (Item  : in out Writer;
      Value : Element);
   --  Add Value to the list, writing the header first if it hasn't been.

   procedure Flush (Item : in out Writer);
   --  Write the records kept so far, and the header if it hasn't been
   --  written, so that an empty list is still a list.

   type Reader
     (Source   : not null access
        Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Capacity : System.Storage_Elements.Storage_Count)
   is tagged limited private;
   --  Reads a list from Source. A record longer than Capacity is read
   --  into a buffer of its own length, on the stack.

   function End_Of_List (Item : in out Reader) return Boolean;
   --  True if the list has no more records. An empty stream is an empty
   --  list. Raises Format_Error if the header is wrong.

   procedure Get
     (Item  : in out Reader;
      Value : out Element);
   --  The next record. A record with fewer fields than this program's
   --  Element gets their defaults, and one with more has them skipped
   --  (idle#114). Raises Format_Error at the end of the list, or if the
   --  header or the record is wrong.

private

   type Writer
     (Target   : not null access
        Rose.Interfaces.Streams.Writer.Writer_Object'Class;
      Capacity : System.Storage_Elements.Storage_Count)
   is tagged limited
      record
         Started : Boolean := False;
         Buffer  : System.Storage_Elements.Storage_Array (1 .. Capacity);
         Last    : System.Storage_Elements.Storage_Offset := 0;
      end record;

   type Reader
     (Source   : not null access
        Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Capacity : System.Storage_Elements.Storage_Count)
   is tagged limited
      record
         Started : Boolean := False;
         Ended   : Boolean := False;
         --  Source has returned nothing.
         Buffer  : System.Storage_Elements.Storage_Array (1 .. Capacity);
         First   : System.Storage_Elements.Storage_Offset := 1;
         --  The next byte to read.
         Last    : System.Storage_Elements.Storage_Offset := 0;
         --  The last byte read from Source.
      end record;

end Rose.Typed_Streams;
