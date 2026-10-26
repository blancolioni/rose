with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Words;

with Rose.Interfaces.File.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Writable_File.Client.Conversions;

with Tests.Registry;

package body Tests.Files is

   use System.Storage_Elements;

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

      Seek (File, 0);
      Read (File, Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello then
         Ada.Text_IO.Put (": wrong contents");
         return False;
      end if;

      Read (File, Buffer, Last);
      if Last /= 0 then
         Ada.Text_IO.Put (": read past the end");
         return False;
      end if;

      --  A read-only client reads the same file, and has no write cap
      --  to give away.

      Read_Only := Conversions.To_File (File);
      Rose.Interfaces.File.Client.Seek (Read_Only, Rose.Words.Word_64'(0));
      Rose.Interfaces.File.Client.Read (Read_Only, Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello
        or else Rose.Interfaces.File.Client.Size (Read_Only)
          /= Hello'Length
      then
         Ada.Text_IO.Put (": wrong contents through read-only client");
         return False;
      end if;

      return True;
   end File_Test;

end Tests.Files;
