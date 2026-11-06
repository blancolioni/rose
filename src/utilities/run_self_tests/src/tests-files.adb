with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.File.Client;
with Rose.Interfaces.File_Control.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Releasable_Writer.Client;
with Rose.Interfaces.Seekable_Reader.Client;
with Rose.Interfaces.Transactional_Writable_File;
with Rose.Interfaces.Writable_File.Client;

with Tests.Registry;

package body Tests.Files is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   package Readers renames Rose.Interfaces.Releasable_Reader.Client;
   package Seekable_Readers renames
     Rose.Interfaces.Seekable_Reader.Client;
   package Writers renames Rose.Interfaces.Releasable_Writer.Client;

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
   subtype Version_Class is
     Rose.Interfaces.Transactional_Writable_File
       .Transactional_Writable_File_Object'Class;

   function Refused
     (File : in out Writable_File_Class)
      return Boolean;
   --  True if File refuses to make a version.

   function Reader_Cap
     (File : in out Rose.Interfaces.File.File_Object'Class)
      return Rose.Capabilities.Capability;
   --  A new reader of File. The test holds its readers as clients, so
   --  that closing one deletes the test's caps for it too.

   function Seekable_Reader_Cap
     (File : in out Rose.Interfaces.File.File_Object'Class)
      return Rose.Capabilities.Capability;
   --  A new seekable reader of File.

   function Writer_Cap
     (File  : in out Writable_File_Class;
      Empty : Boolean := False)
      return Rose.Capabilities.Capability;
   --  A new writer of File, from Rewrite if Empty is True, and from
   --  Append if not. The test holds its writers as clients, as it does
   --  its readers.

   ------------
   -- Append --
   ------------

   procedure Append
     (File : in out Writable_File_Class;
      Data : Storage_Array)
   is
      Writer : Writers.Releasable_Writer_Client;
   begin
      Writer.Open (Writer_Cap (File));
      Writer.Write (Data);
      Writer.Close;
   end Append;

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

      Append (File, Hello);
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

      --  Rewriting empties the file, and moves a reader that was past
      --  the new end back to it, so that it reads what is written next
      --  (ADR 0009, section 4).

      Seekable.Seek (Rose.Words.Word_64'(4));
      declare
         Writer : Writers.Releasable_Writer_Client;
      begin
         Writer.Open (Writer_Cap (File, Empty => True));
         if File.Size /= 0 then
            Ada.Text_IO.Put (": wrong size after rewrite");
            return False;
         end if;
         Writer.Write (Hello);
         Writer.Close;
      end;

      Seekable.Read (Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello then
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
      --  still held for it (ADR 0010): the read fails.

      Second.Release;
      begin
         Second.Read (Buffer, Last);
         Ada.Text_IO.Put (": a released reader can still be read");
         return False;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            null;
      end;

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

      --  More readers at once than a server held before #754.

      declare
         Many : array (1 .. 20) of Readers.Releasable_Reader_Client;
      begin
         for Pass in 1 .. 2 loop
            for Reader of Many loop
               Reader.Open (Reader_Cap (File));
            end loop;
            for Reader of Many loop
               Reader.Read (Buffer, Last);
               if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello
               then
                  Ada.Text_IO.Put (": wrong contents with many readers");
                  return False;
               end if;
            end loop;
            for Reader of Many loop
               Reader.Close;
            end loop;
         end loop;
      end;

      --  A version starts as a copy of the file, and changes it only when
      --  it is committed, as often as its holder likes. Rollback drops
      --  what wasn't committed, and so does Release, after which the
      --  version's caps fail. A file has one version at a time, and a
      --  version has none (ADR 0026).

      declare
         Version : Version_Class := File.New_Version;
      begin
         if Version.Size /= File.Size then
            Ada.Text_IO.Put (": a version is not a copy of its file");
            return False;
         end if;

         Append (Version, Hello);
         if File.Size /= Hello'Length
           or else Version.Size /= 2 * Hello'Length
         then
            Ada.Text_IO.Put (": writing a version changed its file");
            return False;
         end if;

         if not Refused (File) then
            Ada.Text_IO.Put (": a file has two versions");
            return False;
         elsif not Refused (Version) then
            Ada.Text_IO.Put (": a version has a version");
            return False;
         end if;

         Version.Commit;
         if File.Size /= 2 * Hello'Length then
            Ada.Text_IO.Put (": a committed version is not the file");
            return False;
         end if;

         Append (Version, Hello);
         Version.Commit;
         if File.Size /= 3 * Hello'Length
           or else Version.Size /= 3 * Hello'Length
         then
            Ada.Text_IO.Put (": a second commit is not the file");
            return False;
         end if;

         Rewrite (Version, Hello);
         Version.Rollback;
         if Version.Size /= 3 * Hello'Length
           or else File.Size /= 3 * Hello'Length
         then
            Ada.Text_IO.Put (": a rollback is not the file");
            return False;
         end if;

         Rewrite (Version, Hello);
         Version.Release;
         if File.Size /= 3 * Hello'Length then
            Ada.Text_IO.Put (": a released version changed its file");
            return False;
         end if;

         begin
            Append (Version, Hello);
            Ada.Text_IO.Put (": a released version can still be written");
            return False;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;
      end;

      --  Once its version is released, the file can have another.

      declare
         Version : Version_Class := File.New_Version;
      begin
         if Version.Size /= 3 * Hello'Length then
            Ada.Text_IO.Put (": a second version is not a copy");
            return False;
         end if;
         Version.Release;
      end;

      --  A write past the end of the largest file fails, and writes
      --  nothing.

      declare
         Large  : constant Storage_Array (1 .. 4096) := (others => 0);
         Size   : constant Storage_Count := File.Size;
         Writer : Writers.Releasable_Writer_Client;
      begin
         Writer.Open (Writer_Cap (File));
         for I in 1 .. 16 loop
            Writer.Write (Large);
         end loop;
         Ada.Text_IO.Put (": a file grew past its largest size");
         return False;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            Writer.Close;
            if File.Size /= Size + 15 * Large'Length then
               Ada.Text_IO.Put (": a refused write changed the file");
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

   -------------
   -- Refused --
   -------------

   function Refused
     (File : in out Writable_File_Class)
      return Boolean
   is
   begin
      declare
         Version : Version_Class := File.New_Version;
      begin
         Version.Release;
      end;
      return False;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return True;
   end Refused;

   -------------
   -- Rewrite --
   -------------

   procedure Rewrite
     (File : in out Writable_File_Class;
      Data : Storage_Array)
   is
      Writer : Writers.Releasable_Writer_Client;
   begin
      Writer.Open (Writer_Cap (File, Empty => True));
      Writer.Write (Data);
      Writer.Close;
   end Rewrite;

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

      Writer  : Writers.Releasable_Writer_Client;

      function Write
        (First, Last : Storage_Offset;
         Writable    : Boolean)
         return Boolean;
      --  Lend Untouched (First .. Last) to the writer's Write, as the
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
         Rose.System_Calls.Initialize_Send (Params, Writer.Get_Write_Cap);
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

      Writer.Open (Writer_Cap (File));
      if not Write (1, 2 * Page_Bytes, Writable => False)
        or else not Write (2 * Page_Bytes + 1, 3 * Page_Bytes,
                           Writable => True)
      then
         Ada.Text_IO.Put (": the write failed");
         return False;
      end if;
      Writer.Close;

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

   ----------------
   -- Writer_Cap --
   ----------------

   function Writer_Cap
     (File  : in out Writable_File_Class;
      Empty : Boolean := False)
      return Rose.Capabilities.Capability
   is
   begin
      if Empty then
         declare
            Writer : Rose.Interfaces.Releasable_Writer
              .Releasable_Writer_Object'Class :=
                File.Rewrite;
         begin
            return Writer.Releasable_Writer_Cap;
         end;
      else
         declare
            Writer : Rose.Interfaces.Releasable_Writer
              .Releasable_Writer_Object'Class :=
                File.Append;
         begin
            return Writer.Releasable_Writer_Cap;
         end;
      end if;
   end Writer_Cap;

   -----------------
   -- Writer_Test --
   -----------------

   function Writer_Test return Boolean is
      No_Caps : Rose.Capabilities.Capability_Array (1 .. 0);
      Control : Rose.Interfaces.File_Control.Client.File_Control_Client;
      File    : Rose.Interfaces.Writable_File.Client.Writable_File_Client;
      First   : Writers.Releasable_Writer_Client;
      Second  : Writers.Releasable_Writer_Client;
      Third   : Writers.Releasable_Writer_Client;

      function Holds (Expected : Storage_Array) return Boolean;
      --  True if File holds Expected, read through a reader of its own.

      function Refused
        (Writer : in out Writers.Releasable_Writer_Client)
         return Boolean;
      --  True if a write through Writer fails.

      -----------
      -- Holds --
      -----------

      function Holds (Expected : Storage_Array) return Boolean is
         Reader : Readers.Releasable_Reader_Client;
         Buffer : Storage_Array (1 .. 16);
         Last   : Storage_Count;
      begin
         Reader.Open (Reader_Cap (File));
         Reader.Read (Buffer, Last);
         Reader.Close;
         return Buffer (1 .. Last) = Expected;
      end Holds;

      -------------
      -- Refused --
      -------------

      function Refused
        (Writer : in out Writers.Releasable_Writer_Client)
         return Boolean
      is
      begin
         Writer.Write (Hello);
         return False;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return True;
      end Refused;

   begin
      Ada.Text_IO.Put ("Writer test");

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

      --  Two writers at once: every write goes at the end of the file,
      --  whichever writer makes it.

      First.Open (Writer_Cap (File));
      Second.Open (Writer_Cap (File));
      First.Write (Hello (1 .. 2));
      Second.Write (Hello (3 .. 4));
      First.Write (Hello (5 .. 5));
      if not Holds (Hello) then
         Ada.Text_IO.Put (": two writers did not write at the end");
         return False;
      end if;

      --  A rewrite empties the file under the other writers, which go on
      --  writing at its new end.

      Third.Open (Writer_Cap (File, Empty => True));
      if File.Size /= 0 then
         Ada.Text_IO.Put (": a rewrite did not empty the file");
         return False;
      end if;

      Third.Write (Hello (1 .. 2));
      Second.Write (Hello (3 .. 5));
      if not Holds (Hello) then
         Ada.Text_IO.Put (": wrong contents after a rewrite");
         return False;
      end if;

      --  A released writer's caps fail, and the other writers still
      --  write.

      First.Release;
      if not Refused (First) then
         Ada.Text_IO.Put (": a released writer can still write");
         return False;
      end if;

      Second.Write (Hello);
      if File.Size /= 2 * Hello'Length then
         Ada.Text_IO.Put (": releasing a writer stopped another");
         return False;
      end if;
      Second.Close;
      Third.Close;

      --  A version's writers write the version, and end with it. The
      --  file keeps what was committed.

      declare
         Version : Version_Class := File.New_Version;
      begin
         Third.Open (Writer_Cap (Version));
         Third.Write (Hello);
         Version.Commit;
         Third.Write (Hello);
         Version.Release;
         if not Refused (Third) then
            Ada.Text_IO.Put (": a version's writer outlived it");
            return False;
         end if;
      end;

      if File.Size /= 3 * Hello'Length then
         Ada.Text_IO.Put (": wrong size after a version's writes");
         return False;
      end if;

      --  A released writer's object goes back to the pool, to be opened
      --  again.

      for I in 1 .. 20 loop
         Second.Open (Writer_Cap (File));
         if Second.Get_Interface_Cap = Rose.Capabilities.Null_Capability then
            Ada.Text_IO.Put (": no writer after releases");
            return False;
         end if;
         Second.Close;
      end loop;

      return True;
   end Writer_Test;

end Tests.Files;
