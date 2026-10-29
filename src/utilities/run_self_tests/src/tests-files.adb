with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Words;

with Rose.Interfaces.File.Client;
with Rose.Interfaces.File_Reader.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Seekable_File_Reader.Client;
with Rose.Interfaces.Writable_File.Client.Conversions;

with Tests.Registry;

package body Tests.Files is

   use System.Storage_Elements;

   package Readers renames Rose.Interfaces.File_Reader.Client;
   package Seekable_Readers renames
     Rose.Interfaces.Seekable_File_Reader.Client;

   Hello : constant Storage_Array (1 .. 5) :=
     (Character'Pos ('h'), Character'Pos ('e'), Character'Pos ('l'),
      Character'Pos ('l'), Character'Pos ('o'));

   ---------------
   -- File_Test --
   ---------------

   function File_Test return Boolean is
      use Rose.Interfaces.Writable_File.Client;
      use type Rose.Capabilities.Capability;
      Launch    : constant Rose.Interfaces.Launch.Client.Launch_Client :=
        Tests.Registry.Launcher ("file");
      Process   : Rose.Interfaces.Process.Client.Process_Client;
      File      : Writable_File_Client;
      Read_Only : Rose.Interfaces.File.Client.File_Client;
      First     : Readers.File_Reader_Client;
      Second    : Readers.File_Reader_Client;
      Seekable  : Seekable_Readers.Seekable_File_Reader_Client;
      No_Caps   : Rose.Capabilities.Capability_Array (1 .. 0);
      No_Words  : Storage_Array (1 .. 0);
      Buffer    : Storage_Array (1 .. 16);
      Last      : Storage_Count;
   begin
      Ada.Text_IO.Put ("File test");

      Process :=
        Rose.Interfaces.Launch.Client.Launch (Launch, No_Caps, No_Words);
      Open (File, Rose.Interfaces.Process.Client.Published_Interface
                    (Process));

      if Get_Write_Cap (File) = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (": no file server");
         return False;
      end if;

      Write (File, Hello);
      if Size (File) /= Hello'Length then
         Ada.Text_IO.Put (": wrong size after write");
         return False;
      end if;

      --  A reader reads from the start, to the end (ADR 0009).

      First := Open_Reader (File);
      Readers.Read (First, Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello then
         Ada.Text_IO.Put (": wrong contents");
         return False;
      end if;

      Readers.Read (First, Buffer, Last);
      if Last /= 0 then
         Ada.Text_IO.Put (": read past the end");
         return False;
      end if;

      --  Two readers keep their own positions.

      Second := Open_Reader (File);
      Readers.Read (Second, Buffer (1 .. 2), Last);
      Readers.Close (First);
      First := Open_Reader (File);
      Readers.Read (First, Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello then
         Ada.Text_IO.Put (": a reader moved another's position");
         return False;
      end if;
      Readers.Read (Second, Buffer, Last);
      if Last /= 3 or else Buffer (1 .. 3) /= Hello (3 .. 5) then
         Ada.Text_IO.Put (": a reader lost its position");
         return False;
      end if;

      --  A seekable reader seeks.

      Seekable := Open_Seekable_Reader (File);
      Seekable_Readers.Seek (Seekable, Rose.Words.Word_64'(3));
      Seekable_Readers.Read (Seekable, Buffer, Last);
      if Last /= 2 or else Buffer (1 .. 2) /= Hello (4 .. 5) then
         Ada.Text_IO.Put (": wrong contents after a seek");
         return False;
      end if;
      Seekable_Readers.Close (Seekable);

      --  A read-only client opens readers too, and has no write cap to
      --  give away.

      Read_Only := Conversions.To_File (File);
      Readers.Close (First);
      First := Rose.Interfaces.File.Client.Open_Reader (Read_Only);
      Readers.Read (First, Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello
        or else Rose.Interfaces.File.Client.Size (Read_Only)
          /= Hello'Length
      then
         Ada.Text_IO.Put (": wrong contents through read-only client");
         return False;
      end if;
      Readers.Close (First);

      --  A released reader can't be read, through the caps that are
      --  still held for it (ADR 0010).

      Readers.Release (Second);
      Readers.Read (Second, Buffer, Last);
      if Last /= 0 then
         Ada.Text_IO.Put (": a released reader can still be read");
         return False;
      end if;

      --  Released readers give their slots back: there are more readers
      --  here, one at a time, than the file server has slots.

      for I in 1 .. 20 loop
         First := Open_Reader (File);
         if Readers.Get_Interface_Cap (First)
           = Rose.Capabilities.Null_Capability
         then
            Ada.Text_IO.Put (": no reader after releases");
            return False;
         end if;
         Readers.Close (First);
      end loop;

      return True;
   end File_Test;

end Tests.Files;
