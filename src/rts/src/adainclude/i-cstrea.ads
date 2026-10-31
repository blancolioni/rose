--  Rose's Interfaces.C_Streams.
--
--  GCC's package binds the C library's FILE functions, and Rose has no C
--  library. This one has the same specification, less what System.File_IO
--  and Ada.Text_IO don't use, and implements the streams itself. The only
--  streams are the three standard ones, over the caps in
--  Rose.Standard_Streams: fopen fails, until there are files to open.

with System.CRTL;

package Interfaces.C_Streams is
   pragma Preelaborate;

   subtype chars is System.CRTL.chars;
   subtype FILEs is System.CRTL.FILEs;
   subtype int is System.CRTL.int;
   subtype long is System.CRTL.long;
   subtype size_t is System.CRTL.size_t;
   subtype ssize_t is System.CRTL.ssize_t;
   subtype int64 is System.CRTL.int64;
   subtype voids is System.Address;

   NULL_Stream : constant FILEs := System.Null_Address;

   EOF : constant int := -1;
   --  Returned by the input functions at the end of a stream. It is an
   --  event, not a state (ADR 0011, section 3): the next call asks the
   --  stream for more, and a console that has been given more returns it.

   IOFBF : constant int := 0;
   IOLBF : constant int := 1;
   IONBF : constant int := 2;

   L_tmpnam : constant int := 20;

   SEEK_SET : constant int := 0;
   SEEK_CUR : constant int := 1;
   SEEK_END : constant int := 2;

   function stdin  return FILEs;
   function stdout return FILEs;
   function stderr return FILEs;

   procedure clearerr (stream : FILEs);

   function fclose (stream : FILEs) return int;

   function fdopen (handle : int; mode : chars) return FILEs;

   function feof (stream : FILEs) return int;

   function ferror (stream : FILEs) return int;

   function fflush (stream : FILEs) return int;

   function fgetc (stream : FILEs) return int;

   function fgets (strng : chars; n : int; stream : FILEs) return chars;

   function fopen
     (filename : chars;
      mode     : chars;
      encoding : System.CRTL.Filename_Encoding := System.CRTL.UTF8)
     return FILEs;

   function fputc (C : int; stream : FILEs) return int;

   function fread
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t;

   function fread
     (buffer : voids;
      index  : size_t;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t;

   function freopen
     (filename : chars;
      mode     : chars;
      stream   : FILEs;
      encoding : System.CRTL.Filename_Encoding := System.CRTL.UTF8)
     return FILEs;

   function fseek
     (stream : FILEs;
      offset : long;
      origin : int) return int;

   function fseek64
     (stream : FILEs;
      offset : int64;
      origin : int) return int;

   function ftell (stream : FILEs) return long;

   function ftell64 (stream : FILEs) return int64;

   function fwrite
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t;

   function fileno (stream : FILEs) return int;

   function isatty (handle : int) return int;

   procedure rewind (stream : FILEs);

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t) return int;

   procedure tmpnam (str : chars);

   function tmpfile return FILEs;

   function ungetc (c : int; stream : FILEs) return int;

   function unlink (filename : chars) return int;

   function file_exists (name : chars) return int;

   function is_regular_file (handle : int) return int;

   procedure set_binary_mode (handle : int);
   procedure set_text_mode   (handle : int);

   type Content_Encoding is (None, Default_Text, Text, U8text, Wtext, U16text);
   for Content_Encoding use (0,    1,            2,    3,      4,     5);
   pragma Convention (C, Content_Encoding);

   subtype Text_Content_Encoding
     is Content_Encoding range Default_Text .. U16text;

   subtype Non_Default_Text_Content_Encoding
     is Content_Encoding range Text .. U16text;

   procedure set_mode (handle : int; Mode : Content_Encoding);

   procedure full_name (nam : chars; buffer : chars);

   max_path_len : constant Integer := 1024;

end Interfaces.C_Streams;
