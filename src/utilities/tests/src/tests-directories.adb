with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;

with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.File_Reader.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File.Client;

with Tests.Registry;

package body Tests.Directories is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;
   use type Rose.Interfaces.Directory.File_Kind;

   Hello : constant Storage_Array (1 .. 5) :=
     (Character'Pos ('h'), Character'Pos ('e'), Character'Pos ('l'),
      Character'Pos ('l'), Character'Pos ('o'));

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   --------------------
   -- Directory_Test --
   --------------------

   function Directory_Test return Boolean is
      use Rose.Interfaces.Writable_Directory.Client;
      package Writable_Files renames Rose.Interfaces.Writable_File.Client;
      package Read_Only_Files renames Rose.Interfaces.File.Client;
      package Read_Only_Directories renames
        Rose.Interfaces.Directory.Client;

      File_Launch : constant Rose.Interfaces.Launch.Client.Launch_Client :=
        Tests.Registry.Launcher ("file");
      Directory_Launch : constant
        Rose.Interfaces.Launch.Client.Launch_Client :=
          Tests.Registry.Launcher ("directory");
      No_Words    : Storage_Array (1 .. 0);
      Process     : Rose.Interfaces.Process.Client.Process_Client;
      Top         : Writable_Directory_Client;
      Sub         : Writable_Directory_Client;
      Notes       : Writable_Files.Writable_File_Client;
      Inner       : Writable_Files.Writable_File_Client;
      Read_Only   : Read_Only_Files.File_Client;
      Reader      : Rose.Interfaces.File_Reader.Client.File_Reader_Client;
      Sub_Listing : Read_Only_Directories.Directory_Client;
      Name        : String (1 .. 16);
      Name_Last   : Natural;
      Buffer      : Storage_Array (1 .. 16);
      Last        : Storage_Count;
   begin
      Ada.Text_IO.Put ("Directory test");

      Process :=
        Rose.Interfaces.Launch.Client.Launch
          (Directory_Launch,
           (Rose.Interfaces.Launch.Client.Get_Interface_Cap (File_Launch),
            Rose.Interfaces.Launch.Client.Get_Interface_Cap
              (Directory_Launch)),
           No_Words);
      Open (Top, Rose.Interfaces.Process.Client.Published_Interface
                   (Process));

      if Get_Create_File_Cap (Top) = Rose.Capabilities.Null_Capability then
         return Fail ("no directory server");
      end if;

      --  A file, written through its writable interface.

      Notes := Create_File (Top, "notes");
      if Writable_Files.Get_Interface_Cap (Notes)
        = Rose.Capabilities.Null_Capability
      then
         return Fail ("cannot create a file");
      end if;
      Writable_Files.Write (Notes, Hello);

      if Directory_Entry_Count (Top) /= 1
        or else Find_Entry (Top, "notes") /= 1
        or else Find_Entry (Top, "other") /= 0
        or else Directory_Entry_Kind (Top, 1)
          /= Rose.Interfaces.Directory.Ordinary_File
        or else Directory_Entry_Size (Top, 1) /= Hello'Length
      then
         return Fail ("wrong entry for the file");
      end if;

      Directory_Entry_Name (Top, 1, Name, Name_Last);
      if Name (1 .. Name_Last) /= "notes" then
         return Fail ("wrong name");
      end if;

      --  Read back through a reader of the read-only file (ADR 0009).

      Read_Only := Get_Ordinary_File (Top, 1);
      Reader := Read_Only_Files.Open_Reader (Read_Only);
      Rose.Interfaces.File_Reader.Client.Read (Reader, Buffer, Last);
      if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello then
         return Fail ("wrong contents through Get_Ordinary_File");
      end if;
      Rose.Interfaces.File_Reader.Client.Close (Reader);

      --  A subdirectory with a file in it, seen through a read-only
      --  directory.

      Sub := Create_Directory (Top, "sub");
      Inner := Create_File (Sub, "inner");
      if Writable_Files.Get_Interface_Cap (Inner)
        = Rose.Capabilities.Null_Capability
      then
         return Fail ("cannot create a file in a subdirectory");
      end if;

      Sub_Listing := Get_Directory (Top, 2);
      if Read_Only_Directories.Directory_Entry_Count (Sub_Listing) /= 1
        or else Read_Only_Directories.Find_Entry (Sub_Listing, "inner") /= 1
      then
         return Fail ("wrong entries in the subdirectory");
      end if;

      --  The subdirectory is a directory. Asking for it as a file would
      --  return a null cap, and a generated client opens what it is
      --  given, which invokes cap zero (#239), so that is not tested.

      if Directory_Entry_Kind (Top, 2)
        /= Rose.Interfaces.Directory.Directory
      then
         return Fail ("the subdirectory is not a directory");
      end if;

      return True;
   end Directory_Test;

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Message);
      return False;
   end Fail;

end Tests.Directories;
