with System.OS_Lib;
with System.Storage_Elements;

--  System.File_IO is preelaborated, and so is this package, but the caps
--  of the standard streams are not known until a program runs. Nothing here
--  uses them while it is being elaborated.

pragma Warnings (Off, "*categorization*");
pragma Warnings (Off, "*non-preelaborated*");
with Rose.Capabilities;
with Rose.Invocation;
with Rose.Standard_Streams;
with Rose.System_Calls.Client;
pragma Warnings (On, "*non-preelaborated*");
pragma Warnings (On, "*categorization*");

package body Interfaces.C_Streams is

   use type Rose.Capabilities.Capability;
   use type System.Address;
   use type System.CRTL.int64;
   use type System.CRTL.long;
   use type System.CRTL.size_t;
   use type System.Storage_Elements.Storage_Offset;

   LF : constant Character := Character'Val (10);

   Buffer_Length : constant := 1024;

   --  A stream's handle is its index, as an address. The standard streams
   --  have no caps of their own: Cap asks Rose.Standard_Streams for them
   --  when they are used, because a command is only given its streams after
   --  elaboration.

   subtype Stream_Index is Integer range 1 .. 3;

   Standard_Input_Index  : constant Stream_Index := 1;
   Standard_Output_Index : constant Stream_Index := 2;
   Standard_Error_Index  : constant Stream_Index := 3;

   type Stream_State is
      record
         Out_Buffer : String (1 .. Buffer_Length);
         Out_Last   : Natural   := 0;
         In_Buffer  : String (1 .. Buffer_Length);
         In_First   : Positive  := 1;
         In_Last    : Natural   := 0;
         Pushed     : Boolean   := False;
         Pushed_Ch  : Character := LF;
         At_End     : Boolean   := False;
      end record;

   Streams : array (Stream_Index) of Stream_State;

   function Index (Stream : FILEs) return Integer
   is (Integer (System.Storage_Elements.To_Integer (Stream)));

   function Is_Stream (Stream : FILEs) return Boolean
   is (Index (Stream) in Stream_Index);

   function Handle (Index : Stream_Index) return FILEs
   is (System.Storage_Elements.To_Address
         (System.Storage_Elements.Integer_Address (Index)));

   function Cap (Index : Stream_Index) return Rose.Capabilities.Capability
   is (case Index is
          when Standard_Input_Index  => Rose.Standard_Streams.Standard_Input,
          when Standard_Output_Index => Rose.Standard_Streams.Standard_Output,
          when Standard_Error_Index  => Rose.Standard_Streams.Standard_Error);

   procedure Fill (Index : Stream_Index);
   --  Ask the stream for more input. In_Last is 0 afterwards if there is
   --  none.

   procedure Flush (Index : Stream_Index);
   --  Send what has been written to the stream.

   procedure Put (Index : Stream_Index; Item : Character);

   function Char_At
     (Base   : System.Address;
      Offset : System.Storage_Elements.Storage_Offset)
      return Character;

   function Unlink_Exported
     (filename : chars;
      encoding : System.CRTL.Filename_Encoding)
      return int
     with Export, Convention => C, External_Name => "__gnat_unlink";
   --  System.File_IO deletes a file through System.CRTL.

   --  The C functions that GCC's run-time units import from the C files of
   --  GNAT's run time (sysdep.c, adaint.c and cio.c), and that no C
   --  library has.

   Text_Translation_Required : constant int := 0
     with Export, Convention => C,
          External_Name => "__gnat_text_translation_required";

   function Case_Sensitive_Names return int
     with Export, Convention => C,
          External_Name => "__gnat_get_file_names_case_sensitive";

   function Is_Fifo (Path : chars) return int
     with Export, Convention => C, External_Name => "__gnat_is_fifo";

   function Is_File_Not_Found_Error (Err : int) return int
     with Export, Convention => C,
          External_Name => "__gnat_is_file_not_found_error";

   procedure Tmp_Name (Buffer : chars)
     with Export, Convention => C, External_Name => "__gnat_tmp_name";

   procedure Get_Immediate
     (stream      : FILEs;
      ch          : out int;
      end_of_file : out int)
     with Export, Convention => C, External_Name => "getc_immediate";

   procedure Get_Immediate_No_Wait
     (stream      : FILEs;
      ch          : out int;
      end_of_file : out int;
      avail       : out int)
     with Export, Convention => C,
          External_Name => "getc_immediate_nowait";

   --------------------------
   -- Case_Sensitive_Names --
   --------------------------

   function Case_Sensitive_Names return int is
   begin
      return 1;
   end Case_Sensitive_Names;

   -------------
   -- Char_At --
   -------------

   function Char_At
     (Base   : System.Address;
      Offset : System.Storage_Elements.Storage_Offset)
      return Character
   is
      Ch : Character with Import, Address => Base + Offset;
   begin
      return Ch;
   end Char_At;

   --------------
   -- clearerr --
   --------------

   procedure clearerr (stream : FILEs) is
   begin
      if Is_Stream (stream) then
         Streams (Index (stream)).At_End := False;
      end if;
   end clearerr;

   ------------
   -- fclose --
   ------------

   function fclose (stream : FILEs) return int is
   begin
      --  The standard streams are not closed, only emptied.
      return fflush (stream);
   end fclose;

   ------------
   -- fdopen --
   ------------

   function fdopen (handle : int; mode : chars) return FILEs is
      pragma Unreferenced (mode);
   begin
      if handle in 0 .. Stream_Index'Last - 1 then
         return C_Streams.Handle (handle + 1);
      else
         System.OS_Lib.Set_Errno (System.OS_Lib.EBADF);
         return NULL_Stream;
      end if;
   end fdopen;

   ----------
   -- feof --
   ----------

   function feof (stream : FILEs) return int is
   begin
      if Is_Stream (stream) and then Streams (Index (stream)).At_End then
         return 1;
      else
         return 0;
      end if;
   end feof;

   ------------
   -- ferror --
   ------------

   function ferror (stream : FILEs) return int is
      pragma Unreferenced (stream);
   begin
      return 0;
   end ferror;

   ----------
   -- Fill --
   ----------

   procedure Fill (Index : Stream_Index) is
      S      : Stream_State renames Streams (Index);
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      S.In_First := 1;
      S.In_Last  := 0;

      --  Text can only be read from standard input, and a program that
      --  has no standard input reads nothing.

      if Index /= Standard_Input_Index
        or else Cap (Index) = Rose.Capabilities.Null_Capability
      then
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Cap (Index));
      Rose.System_Calls.Receive_Buffer (Params, Buffer_Length);
      Rose.System_Calls.Invoke_Capability (Params);
      Rose.System_Calls.Copy_Text
        (Params => Params,
         Count  => Natural (Params.Data (0)),
         To     => S.In_Buffer,
         Last   => S.In_Last);
   end Fill;

   ------------
   -- fflush --
   ------------

   function fflush (stream : FILEs) return int is
   begin
      if stream = NULL_Stream then
         for Index in Stream_Index loop
            Flush (Index);
         end loop;
      elsif Is_Stream (stream) then
         Flush (Index (stream));
      else
         System.OS_Lib.Set_Errno (System.OS_Lib.EBADF);
         return EOF;
      end if;
      return 0;
   end fflush;

   -----------
   -- Flush --
   -----------

   procedure Flush (Index : Stream_Index) is
      S : Stream_State renames Streams (Index);
   begin
      if S.Out_Last > 0 then
         --  Text written to a stream the program does not have is lost.
         if Cap (Index) /= Rose.Capabilities.Null_Capability then
            Rose.System_Calls.Client.Send_String
              (Cap     => Cap (Index),
               Message => S.Out_Buffer (1 .. S.Out_Last));
         end if;
         S.Out_Last := 0;
      end if;
   end Flush;

   -----------
   -- fgetc --
   -----------

   function fgetc (stream : FILEs) return int is
   begin
      if not Is_Stream (stream) then
         System.OS_Lib.Set_Errno (System.OS_Lib.EBADF);
         return EOF;
      end if;

      declare
         S  : Stream_State renames Streams (Index (stream));
         Ch : Character;
      begin
         if S.Pushed then
            S.Pushed := False;
            return Character'Pos (S.Pushed_Ch);
         end if;

         if S.In_First > S.In_Last then
            Fill (Index (stream));
            if S.In_Last = 0 then
               S.At_End := True;
               return EOF;
            end if;
         end if;

         Ch := S.In_Buffer (S.In_First);
         S.In_First := S.In_First + 1;
         return Character'Pos (Ch);
      end;
   end fgetc;

   -----------
   -- fgets --
   -----------

   function fgets (strng : chars; n : int; stream : FILEs) return chars is
      Count : Natural := 0;
   begin
      --  Read up to the end of a line, leaving room for the NUL.

      while Count < n - 1 loop
         declare
            Ch   : constant int := fgetc (stream);
            Item : Character with
              Import,
              Address => strng + System.Storage_Elements.Storage_Offset
                (Count);
         begin
            exit when Ch = EOF;
            Item := Character'Val (Ch);
            Count := Count + 1;
            exit when Item = LF;
         end;
      end loop;

      if Count = 0 then
         return NULL_Stream;
      end if;

      declare
         Last : Character with
           Import,
           Address => strng + System.Storage_Elements.Storage_Offset (Count);
      begin
         Last := Character'Val (0);
      end;
      return strng;
   end fgets;

   -----------------
   -- file_exists --
   -----------------

   function file_exists (name : chars) return int is
      pragma Unreferenced (name);
   begin
      return 0;
   end file_exists;

   ------------
   -- fileno --
   ------------

   function fileno (stream : FILEs) return int is
   begin
      if Is_Stream (stream) then
         return Index (stream) - 1;
      else
         return -1;
      end if;
   end fileno;

   -----------
   -- fopen --
   -----------

   function fopen
     (filename : chars;
      mode     : chars;
      encoding : System.CRTL.Filename_Encoding := System.CRTL.UTF8)
      return FILEs
   is
      pragma Unreferenced (filename, mode, encoding);
   begin
      System.OS_Lib.Set_Errno (System.OS_Lib.ENOENT);
      return NULL_Stream;
   end fopen;

   -----------
   -- fputc --
   -----------

   function fputc (C : int; stream : FILEs) return int is
   begin
      if not Is_Stream (stream) then
         System.OS_Lib.Set_Errno (System.OS_Lib.EBADF);
         return EOF;
      end if;

      Put (Index (stream), Character'Val (C));
      if Index (stream) = Standard_Error_Index then
         Flush (Standard_Error_Index);
      end if;
      return C;
   end fputc;

   -----------
   -- fread --
   -----------

   function fread
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t
   is
   begin
      return fread (buffer, 0, size, count, stream);
   end fread;

   -----------
   -- fread --
   -----------

   function fread
     (buffer : voids;
      index  : size_t;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t
   is
      Total : constant size_t := size * count;
      Done  : size_t := 0;
   begin
      while Done < Total loop
         declare
            Ch : constant int := fgetc (stream);
            Item : Character with
              Import,
              Address => buffer + System.Storage_Elements.Storage_Offset
                (index + Done);
         begin
            exit when Ch = EOF;
            Item := Character'Val (Ch);
            Done := Done + 1;
         end;
      end loop;

      if size = 0 then
         return 0;
      else
         return Done / size;
      end if;
   end fread;

   -------------
   -- freopen --
   -------------

   function freopen
     (filename : chars;
      mode     : chars;
      stream   : FILEs;
      encoding : System.CRTL.Filename_Encoding := System.CRTL.UTF8)
      return FILEs
   is
      pragma Unreferenced (filename, mode, stream, encoding);
   begin
      System.OS_Lib.Set_Errno (System.OS_Lib.ENOENT);
      return NULL_Stream;
   end freopen;

   -----------
   -- fseek --
   -----------

   function fseek
     (stream : FILEs;
      offset : long;
      origin : int) return int
   is
      pragma Unreferenced (stream, offset, origin);
   begin
      System.OS_Lib.Set_Errno (System.OS_Lib.EINVAL);
      return -1;
   end fseek;

   -------------
   -- fseek64 --
   -------------

   function fseek64
     (stream : FILEs;
      offset : int64;
      origin : int) return int
   is
      pragma Unreferenced (stream, offset, origin);
   begin
      System.OS_Lib.Set_Errno (System.OS_Lib.EINVAL);
      return -1;
   end fseek64;

   -----------
   -- ftell --
   -----------

   function ftell (stream : FILEs) return long is
      pragma Unreferenced (stream);
   begin
      System.OS_Lib.Set_Errno (System.OS_Lib.EINVAL);
      return -1;
   end ftell;

   -------------
   -- ftell64 --
   -------------

   function ftell64 (stream : FILEs) return int64 is
      pragma Unreferenced (stream);
   begin
      System.OS_Lib.Set_Errno (System.OS_Lib.EINVAL);
      return -1;
   end ftell64;

   ---------------
   -- full_name --
   ---------------

   procedure full_name (nam : chars; buffer : chars) is
      Offset : System.Storage_Elements.Storage_Offset := 0;
   begin
      loop
         declare
            Ch : constant Character := Char_At (nam, Offset);
            Item : Character with Import, Address => buffer + Offset;
         begin
            Item := Ch;
            exit when Ch = Character'Val (0);
            Offset := Offset + 1;
         end;
      end loop;
   end full_name;

   ------------
   -- fwrite --
   ------------

   function fwrite
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t
   is
      Total : constant size_t := size * count;
   begin
      if not Is_Stream (stream) then
         System.OS_Lib.Set_Errno (System.OS_Lib.EBADF);
         return 0;
      end if;

      for I in 1 .. Total loop
         Put (Index (stream),
              Char_At (buffer,
                       System.Storage_Elements.Storage_Offset (I - 1)));
      end loop;

      if Index (stream) = Standard_Error_Index then
         Flush (Standard_Error_Index);
      end if;

      return count;
   end fwrite;

   ------------
   -- isatty --
   ------------

   function isatty (handle : int) return int is
   begin
      return (if handle in 0 .. Stream_Index'Last - 1 then 1 else 0);
   end isatty;

   ---------------------
   -- is_regular_file --
   ---------------------

   function is_regular_file (handle : int) return int is
      pragma Unreferenced (handle);
   begin
      return 0;
   end is_regular_file;

   ---------
   -- Put --
   ---------

   procedure Put (Index : Stream_Index; Item : Character) is
      S : Stream_State renames Streams (Index);
   begin
      S.Out_Last := S.Out_Last + 1;
      S.Out_Buffer (S.Out_Last) := Item;
      if Item = LF or else S.Out_Last = Buffer_Length then
         Flush (Index);
      end if;
   end Put;

   ------------
   -- rewind --
   ------------

   procedure rewind (stream : FILEs) is
   begin
      clearerr (stream);
   end rewind;

   ---------------------
   -- set_binary_mode --
   ---------------------

   procedure set_binary_mode (handle : int) is
      pragma Unreferenced (handle);
   begin
      null;
   end set_binary_mode;

   --------------
   -- set_mode --
   --------------

   procedure set_mode (handle : int; Mode : Content_Encoding) is
      pragma Unreferenced (handle, Mode);
   begin
      null;
   end set_mode;

   -------------------
   -- set_text_mode --
   -------------------

   procedure set_text_mode (handle : int) is
      pragma Unreferenced (handle);
   begin
      null;
   end set_text_mode;

   -------------
   -- setvbuf --
   -------------

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t) return int
   is
      pragma Unreferenced (stream, buffer, mode, size);
   begin
      --  Output is sent at the end of a line, or when the buffer is full.
      return 0;
   end setvbuf;

   ------------
   -- stderr --
   ------------

   function stderr return FILEs is
   begin
      return Handle (Standard_Error_Index);
   end stderr;

   -----------
   -- stdin --
   -----------

   function stdin return FILEs is
   begin
      return Handle (Standard_Input_Index);
   end stdin;

   ------------
   -- stdout --
   ------------

   function stdout return FILEs is
   begin
      return Handle (Standard_Output_Index);
   end stdout;

   ------------
   -- tmpfile --
   ------------

   function tmpfile return FILEs is
   begin
      System.OS_Lib.Set_Errno (System.OS_Lib.ENOSYS);
      return NULL_Stream;
   end tmpfile;

   ------------
   -- tmpnam --
   ------------

   procedure tmpnam (str : chars) is
      pragma Unreferenced (str);
   begin
      null;
   end tmpnam;

   ------------
   -- ungetc --
   ------------

   function ungetc (c : int; stream : FILEs) return int is
   begin
      if c = EOF or else not Is_Stream (stream) then
         return EOF;
      end if;

      declare
         S : Stream_State renames Streams (Index (stream));
      begin
         S.Pushed_Ch := Character'Val (c);
         S.Pushed    := True;
         S.At_End    := False;
      end;
      return c;
   end ungetc;

   ------------
   -- unlink --
   ------------

   function unlink (filename : chars) return int is
      pragma Unreferenced (filename);
   begin
      System.OS_Lib.Set_Errno (System.OS_Lib.ENOENT);
      return -1;
   end unlink;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate
     (stream      : FILEs;
      ch          : out int;
      end_of_file : out int)
   is
   begin
      ch := fgetc (stream);
      end_of_file := (if ch = EOF then 1 else 0);
   end Get_Immediate;

   ---------------------------
   -- Get_Immediate_No_Wait --
   ---------------------------

   procedure Get_Immediate_No_Wait
     (stream      : FILEs;
      ch          : out int;
      end_of_file : out int;
      avail       : out int)
   is
      Ready : constant Boolean :=
                Is_Stream (stream)
                and then (Streams (Index (stream)).Pushed
                          or else Streams (Index (stream)).In_First
                                    <= Streams (Index (stream)).In_Last);
   begin
      --  There is no way to ask a stream whether it has input without
      --  waiting for it, so only what has already been read is available.

      if Ready then
         ch := fgetc (stream);
         avail := 1;
      else
         ch := 0;
         avail := 0;
      end if;
      end_of_file := 0;
   end Get_Immediate_No_Wait;

   -------------
   -- Is_Fifo --
   -------------

   function Is_Fifo (Path : chars) return int is
      pragma Unreferenced (Path);
   begin
      return 0;
   end Is_Fifo;

   -----------------------------
   -- Is_File_Not_Found_Error --
   -----------------------------

   function Is_File_Not_Found_Error (Err : int) return int is
   begin
      return (if Err = System.OS_Lib.ENOENT then 1 else 0);
   end Is_File_Not_Found_Error;

   --------------
   -- Tmp_Name --
   --------------

   procedure Tmp_Name (Buffer : chars) is
      First : Character with Import, Address => Buffer;
   begin
      --  There is nowhere to make a temporary file yet.
      First := Character'Val (0);
   end Tmp_Name;

   ---------------------
   -- Unlink_Exported --
   ---------------------

   function Unlink_Exported
     (filename : chars;
      encoding : System.CRTL.Filename_Encoding)
      return int
   is
      pragma Unreferenced (encoding);
   begin
      return unlink (filename);
   end Unlink_Exported;

end Interfaces.C_Streams;
