with Ada.Streams;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Interfaces.Streams.Reader;
with Rose.Interfaces.Streams.Writer;

package Test_Pipes is

   --  A pipe in memory for the tests of Rose.Typed_Streams (#140), whose
   --  Read gives at most Chunk bytes, as a pipe between processes gives
   --  what has arrived.

   use Ada.Streams;

   type Pipe is
     limited new Rose.Interfaces.Streams.Writer.Writer_Object
     and Rose.Interfaces.Streams.Reader.Reader_Object with
      record
         Data  : Stream_Element_Array (1 .. 65_536);
         Last  : Stream_Element_Offset := 0;
         Next  : Stream_Element_Offset := 1;
         Chunk : Stream_Element_Offset := 7;
      end record;

   overriding function Writer_Cap
     (Item : in out Pipe)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   overriding function Reader_Cap
     (Item : in out Pipe)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   overriding procedure Write
     (Item   : in out Pipe;
      Buffer : Stream_Element_Array);

   overriding procedure Read
     (Item   : in out Pipe;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   procedure Put_Bytes
     (Item  : in out Pipe;
      Bytes : System.Storage_Elements.Storage_Array);
   --  Write Bytes to Item, as a producer that isn't this one would.

   procedure Put_Word
     (Item  : in out Pipe;
      Value : Natural);
   --  A little-endian word.

end Test_Pipes;
