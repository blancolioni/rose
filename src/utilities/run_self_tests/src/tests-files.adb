with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.File.Client;
with Rose.Interfaces.File_Control.Client;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Seekable_Reader.Client;
with Rose.Interfaces.Writable_File.Client;

with Tests.Registry;

package body Tests.Files is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   package Readers renames Rose.Interfaces.Releasable_Reader.Client;
   package Seekable_Readers renames
     Rose.Interfaces.Seekable_Reader.Client;

   Hello : constant Storage_Array (1 .. 5) :=
     (Character'Pos ('h'), Character'Pos ('e'), Character'Pos ('l'),
      Character'Pos ('l'), Character'Pos ('o'));

   Page_Bytes : constant := 4096;

   Untouched : Storage_Array (1 .. 3 * Page_Bytes)
     with Alignment => Page_Bytes;
   --  Never read or written by the test, so none of its pages is mapped
   --  until the kernel asks mem for them (#713).

   subtype Writable_File_Class is
     Rose.Interfaces.Writable_File.Writable_File_Object'Class;

   function Reader_Cap
     (File : in out Rose.Interfaces.File.File_Object'Class)
      return Rose.Capabilities.Capability;
   --  A new reader of File. The test holds its readers as clients, so
   --  that closing one deletes the test's caps for it too.

   function Seekable_Reader_Cap
     (File : in out Rose.Interfaces.File.File_Object'Class)
      return Rose.Capabilities.Capability;
   --  A new seekable reader of File.

   ---------------
   -- File_Test --
   ---------------

   function File_Test return Boolean is
      No_Caps   : Rose.Capabilities.Capability_Array (1 .. 0);
      Control   : Rose.Interfaces.File_Control.Client.File_Control_Client;
      File      : Rose.Interfaces.Writable_File.Client.Writable_File_Client;
      Read_Only : Rose.Interfaces.File.Client.File_Client;
      First     : Readers.Releasable_Reader_Client;
      Second    : Readers.Releasable_Reader_Client;
      Seekable  : Seekable_Readers.Seekable_Reader_Client;
      Buffer    : Storage_Array (1 .. 16);
      Last      : Storage_Count;
   begin
      Ada.Text_IO.Put ("File test");

      --  The file's process publishes File_Control, which gives the file
      --  itself (#141).

      Control.Open (Tests.Registry.Launch ("file", No_Caps));

      if Control.Get_Interface_Cap = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (": no file server");
         return False;
      end if;

      declare
         Writable : Writable_File_Class := Control.Writable;
      begin
         File.Open (Writable.Writable_File_Cap);
      end;

      File.Write (Hello);
      if File.Size /= Hello'Length then
         Ada.Text_IO.Put (": wrong size after write");
         return False;
      end if;

      --  A reader reads from the start, to the end (ADR 0009).

      First.Open (Reader_Cap (File));
      First.Read (Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello then
         Ada.Text_IO.Put (": wrong contents");
         return False;
      end if;

      First.Read (Buffer, Last);
      if Last /= 0 then
         Ada.Text_IO.Put (": read past the end");
         return False;
      end if;

      --  Two readers keep their own positions.

      Second.Open (Reader_Cap (File));
      Second.Read (Buffer (1 .. 2), Last);
      First.Close;
      First.Open (Reader_Cap (File));
      First.Read (Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello then
         Ada.Text_IO.Put (": a reader moved another's position");
         return False;
      end if;
      Second.Read (Buffer, Last);
      if Last /= 3 or else Buffer (1 .. 3) /= Hello (3 .. 5) then
         Ada.Text_IO.Put (": a reader lost its position");
         return False;
      end if;

      --  A seekable reader seeks.

      Seekable.Open (Seekable_Reader_Cap (File));
      Seekable.Seek (Rose.Words.Word_64'(3));
      Seekable.Read (Buffer, Last);
      if Last /= 2 or else Buffer (1 .. 2) /= Hello (4 .. 5) then
         Ada.Text_IO.Put (": wrong contents after a seek");
         return False;
      end if;

      --  Truncating shortens the file, and moves a reader that was past
      --  the new end back to it, so that it reads what is written next
      --  (#347). A larger size leaves the file as it is.

      Seekable.Seek (Rose.Words.Word_64'(4));
      File.Truncate (2);
      if File.Size /= 2 then
         Ada.Text_IO.Put (": wrong size after truncate");
         return False;
      end if;

      File.Truncate (Hello'Length);
      if File.Size /= 2 then
         Ada.Text_IO.Put (": truncate made the file longer");
         return False;
      end if;

      File.Write (Hello (3 .. 5));
      Seekable.Read (Buffer, Last);
      if Last /= 3 or else Buffer (1 .. 3) /= Hello (3 .. 5) then
         Ada.Text_IO.Put (": a reader past the end did not move back");
         return False;
      end if;
      Seekable.Close;

      --  A read-only client opens readers too, and has no write cap to
      --  give away.

      Read_Only.Open (File.File_Cap);
      First.Close;
      First.Open (Reader_Cap (Read_Only));
      First.Read (Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello
        or else Read_Only.Size /= Hello'Length
      then
         Ada.Text_IO.Put (": wrong contents through read-only client");
         return False;
      end if;
      First.Close;

      --  A released reader can't be read, through the caps that are
      --  still held for it (ADR 0010).

      Second.Release;
      Second.Read (Buffer, Last);
      if Last /= 0 then
         Ada.Text_IO.Put (": a released reader can still be read");
         return False;
      end if;

      --  Released readers give their slots back: there are more readers
      --  here, one at a time, than the file server has slots.

      for I in 1 .. 20 loop
         First.Open (Reader_Cap (File));
         if First.Get_Interface_Cap = Rose.Capabilities.Null_Capability then
            Ada.Text_IO.Put (": no reader after releases");
            return False;
         end if;
         First.Close;
      end loop;

      --  A version starts as a copy of the file, and changes it only when
      --  it is committed. A file has one version at a time, and the caps
      --  of a committed version stop working (#141).

      declare
         Version : Writable_File_Class := Control.New_Version;
      begin
         if Version.Size /= File.Size then
            Ada.Text_IO.Put (": a version is not a copy of its file");
            return False;
         end if;

         Version.Write (Hello);
         if File.Size /= Hello'Length
           or else Version.Size /= 2 * Hello'Length
         then
            Ada.Text_IO.Put (": writing a version changed its file");
            return False;
         end if;

         declare
            Again : Writable_File_Class := Control.New_Version;
         begin
            if Again.Writable_File_Cap /= Rose.Capabilities.Null_Capability
            then
               Ada.Text_IO.Put (": a file has two versions");
               return False;
            end if;
         end;

         Control.Commit_Version;
         if File.Size /= 2 * Hello'Length then
            Ada.Text_IO.Put (": a committed version is not the file");
            return False;
         end if;

         Version.Write (Hello);
         if File.Size /= 2 * Hello'Length then
            Ada.Text_IO.Put (": a committed version can still be written");
            return False;
         end if;
      end;

      --  Discarding a version leaves the file as it was.

      declare
         Version : Writable_File_Class := Control.New_Version;
      begin
         Version.Truncate (0);
         Control.Discard_Version;
         if File.Size /= 2 * Hello'Length then
            Ada.Text_IO.Put (": a discarded version changed its file");
            return False;
         end if;
      end;

      return True;
   end File_Test;

   ----------------
   -- Reader_Cap --
   ----------------

   function Reader_Cap
     (File : in out Rose.Interfaces.File.File_Object'Class)
      return Rose.Capabilities.Capability
   is
      Reader : Rose.Interfaces.Releasable_Reader
        .Releasable_Reader_Object'Class :=
        File.Open_Reader;
   begin
      return Reader.Releasable_Reader_Cap;
   end Reader_Cap;

   -------------------------
   -- Seekable_Reader_Cap --
   -------------------------

   function Seekable_Reader_Cap
     (File : in out Rose.Interfaces.File.File_Object'Class)
      return Rose.Capabilities.Capability
   is
      Reader : Rose.Interfaces.Seekable_Reader
        .Seekable_Reader_Object'Class :=
          File.Open_Seekable_Reader;
   begin
      return Reader.Seekable_Reader_Cap;
   end Seekable_Reader_Cap;

   ---------------------------
   -- Untouched_Buffer_Test --
   ---------------------------

   function Untouched_Buffer_Test return Boolean is
      No_Caps : Rose.Capabilities.Capability_Array (1 .. 0);
      Control : Rose.Interfaces.File_Control.Client.File_Control_Client;
      File    : Rose.Interfaces.Writable_File.Client.Writable_File_Client;
      Reader  : Readers.Releasable_Reader_Client;
      Buffer  : Storage_Array (1 .. 256);
      Last    : Storage_Count;
      Total   : Storage_Count := 0;

      function Write
        (First, Last : Storage_Offset;
         Writable    : Boolean)
         return Boolean;
      --  Lend Untouched (First .. Last) to the file's Write, as the
      --  generated client does not: it copies into its own buffer.

      -----------
      -- Write --
      -----------

      function Write
        (First, Last : Storage_Offset;
         Writable    : Boolean)
         return Boolean
      is
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, File.Get_Write_Cap);
         Rose.System_Calls.Send_Buffer
           (Params, Last - First + 1, Untouched (First)'Address, Writable);
         Rose.System_Calls.Invoke_Capability (Params);
         return not Params.Control.Flags (Rose.Invocation.Error);
      end Write;

   begin
      Ada.Text_IO.Put ("Untouched buffer test");

      Control.Open (Tests.Registry.Launch ("file", No_Caps));

      if Control.Get_Interface_Cap = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (": no file server");
         return False;
      end if;

      declare
         Writable : Writable_File_Class := Control.Writable;
      begin
         File.Open (Writable.Writable_File_Cap);
      end;

      --  Two pages read-only, and the third writable, which mem maps for
      --  writing.

      if not Write (1, 2 * Page_Bytes, Writable => False)
        or else not Write (2 * Page_Bytes + 1, 3 * Page_Bytes,
                           Writable => True)
      then
         Ada.Text_IO.Put (": the write failed");
         return False;
      end if;

      if File.Size /= Untouched'Length then
         Ada.Text_IO.Put (": wrong size");
         return False;
      end if;

      Reader.Open (Reader_Cap (File));
      loop
         Reader.Read (Buffer, Last);
         exit when Last = 0;
         if Buffer (1 .. Last) /= (1 .. Last => 0) then
            Ada.Text_IO.Put (": the server read something else");
            return False;
         end if;
         Total := Total + Last;
      end loop;
      Reader.Close;

      if Total /= Untouched'Length then
         Ada.Text_IO.Put (": read back the wrong length");
         return False;
      end if;

      return True;
   end Untouched_Buffer_Test;

end Tests.Files;
