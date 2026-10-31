with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Words;

with Rose.Interfaces.File.Client;
with Rose.Interfaces.File_Reader.Client;
with Rose.Interfaces.Seekable_File_Reader.Client;
with Rose.Interfaces.Writable_File.Client;

with Tests.Registry;

package body Tests.Files is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   package Readers renames Rose.Interfaces.File_Reader.Client;
   package Seekable_Readers renames
     Rose.Interfaces.Seekable_File_Reader.Client;

   Hello : constant Storage_Array (1 .. 5) :=
     (Character'Pos ('h'), Character'Pos ('e'), Character'Pos ('l'),
      Character'Pos ('l'), Character'Pos ('o'));

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
      File      : Rose.Interfaces.Writable_File.Client.Writable_File_Client;
      Read_Only : Rose.Interfaces.File.Client.File_Client;
      First     : Readers.File_Reader_Client;
      Second    : Readers.File_Reader_Client;
      Seekable  : Seekable_Readers.Seekable_File_Reader_Client;
      Buffer    : Storage_Array (1 .. 16);
      Last      : Storage_Count;
   begin
      Ada.Text_IO.Put ("File test");

      File.Open (Tests.Registry.Launch ("file", No_Caps));

      if File.Writable_File_Cap = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (": no file server");
         return False;
      end if;

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

      return True;
   end File_Test;

   ----------------
   -- Reader_Cap --
   ----------------

   function Reader_Cap
     (File : in out Rose.Interfaces.File.File_Object'Class)
      return Rose.Capabilities.Capability
   is
      Reader : Rose.Interfaces.File_Reader.File_Reader_Object'Class :=
        File.Open_Reader;
   begin
      return Reader.File_Reader_Cap;
   end Reader_Cap;

   -------------------------
   -- Seekable_Reader_Cap --
   -------------------------

   function Seekable_Reader_Cap
     (File : in out Rose.Interfaces.File.File_Object'Class)
      return Rose.Capabilities.Capability
   is
      Reader : Rose.Interfaces.Seekable_File_Reader
        .Seekable_File_Reader_Object'Class :=
          File.Open_Seekable_Reader;
   begin
      return Reader.Seekable_File_Reader_Cap;
   end Seekable_Reader_Cap;

end Tests.Files;
