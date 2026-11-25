with Ada.Streams;

with Rose.Capabilities;
with Rose.Words;

with Rose.Interfaces.Streams.Releasable_Writer;
with Rose.Interfaces.Streams.Seekable_Reader;

private with Ada.Streams.Stream_IO;

package Rose.Host.Streams is

   --  Readers and writers over the host's files and standard streams.

   type Reader is limited new
     Rose.Interfaces.Streams.Seekable_Reader.Seekable_Reader_Object
     with private;
   --  A reader of a file, or of standard input. Release closes it.

   procedure Open
     (Item : in out Reader;
      Name : String);
   --  The backing of a reader argument: standard input if Name is "",
   --  which is a reader whose default is Standard_Input, not given, and
   --  otherwise the file Name. Raises Command_Failed if there is none.

   procedure Open_Path
     (Item : in out Reader;
      Path : String);
   --  Read the file Path. Raises Invocation_Error if it can't be opened.

   overriding function Reader_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability;

   overriding procedure Read
     (Item   : in out Reader;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  The next part of the stream. Last is Buffer'First - 1 at its end.
   --  From standard input, a part is what is there to read, as on Rose.

   overriding function Resource_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability;

   overriding procedure Release (Item : in out Reader);

   overriding function Releasable_Reader_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability;

   overriding function Seekable_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability;

   overriding procedure Seek
     (Item              : in out Reader;
      Offset_From_Start : Rose.Words.Word_64);
   --  Raises Invocation_Error for standard input.

   overriding function Seekable_Reader_Cap
     (Item : in out Reader)
      return Rose.Capabilities.Capability;

   type Writer is limited new
     Rose.Interfaces.Streams.Releasable_Writer.Releasable_Writer_Object
     with private;
   --  A writer of a file, or of standard output. Release closes it.

   procedure Open
     (Item : in out Writer;
      Name : String);
   --  The backing of a writer argument or role member: standard output if
   --  Name is "", and otherwise the file Name, made empty, or made if
   --  there is none. Raises Command_Failed if it can't be written.

   procedure Open_Path
     (Item   : in out Writer;
      Path   : String;
      Append : Boolean);
   --  Write the file Path, after what it holds if Append is True, and in
   --  place of it otherwise. Raises Invocation_Error if it can't be
   --  opened.

   overriding function Writer_Cap
     (Item : in out Writer)
      return Rose.Capabilities.Capability;

   overriding procedure Write
     (Item   : in out Writer;
      Buffer : Ada.Streams.Stream_Element_Array);

   overriding function Resource_Cap
     (Item : in out Writer)
      return Rose.Capabilities.Capability;

   overriding procedure Release (Item : in out Writer);

   overriding function Releasable_Writer_Cap
     (Item : in out Writer)
      return Rose.Capabilities.Capability;

private

   type Reader is limited new
     Rose.Interfaces.Streams.Seekable_Reader.Seekable_Reader_Object with
      record
         Standard_Input : Boolean := False;
         File           : Ada.Streams.Stream_IO.File_Type;
      end record;

   type Writer is limited new
     Rose.Interfaces.Streams.Releasable_Writer.Releasable_Writer_Object with
      record
         Standard_Output : Boolean := False;
         File            : Ada.Streams.Stream_IO.File_Type;
      end record;

end Rose.Host.Streams;
