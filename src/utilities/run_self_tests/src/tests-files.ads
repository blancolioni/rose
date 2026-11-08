with Ada.Streams;

with Rose.Interfaces.Writable_File;

package Tests.Files is

   function File_Test return Boolean;
   --  Launch a file server, write to it, and read it back through both
   --  the writable client and a read-only one.

   function Writer_Test return Boolean;
   --  Launch a file server, and write to it through several writers at
   --  once (ADR 0009, section 4): their writes go at the end, a rewrite
   --  empties the file under them, and a writer's caps fail once it is
   --  released, or once the version it writes is.

   function Cap_Stream_Test return Boolean;
   --  Write values with 'Write and 'Output through a Rose.Streams stream
   --  over a file's writer, among them a string longer than a page, and
   --  read them back with 'Read and 'Input over a reader of it (#727).
   --  Reading past the end raises End_Error.

   function Untouched_Buffer_Test return Boolean;
   --  Write to a file server from buffers whose pages the test has never
   --  touched, so that mem has not mapped them: one lent read-only, one
   --  lent writable. The kernel has the pages mapped before it shares
   --  them, so the server reads zeroes from them, and not whatever page
   --  an unmapped entry named (#713).

   procedure Append
     (File : in out Rose.Interfaces.Writable_File.Writable_File_Object'Class;
      Data : Ada.Streams.Stream_Element_Array);
   --  Write Data at the end of File, through a writer that is released
   --  once it has written.

   procedure Rewrite
     (File : in out Rose.Interfaces.Writable_File.Writable_File_Object'Class;
      Data : Ada.Streams.Stream_Element_Array);
   --  Replace the contents of File with Data, as Append does.

end Tests.Files;
