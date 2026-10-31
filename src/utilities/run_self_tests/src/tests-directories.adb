with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;

with Rose.Interfaces.Directory;
with Rose.Interfaces.File;
with Rose.Interfaces.File_Reader;
with Rose.Interfaces.Objects;
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
      Top    : Rose.Interfaces.Writable_Directory.Client
        .Writable_Directory_Client;
      Buffer    : Storage_Array (1 .. 16);
      Last      : Storage_Count;
      Notes_Cap : Rose.Capabilities.Capability;
      Held      : Rose.Interfaces.Writable_File.Client
        .Writable_File_Client;

      function Refused (From, To : String) return Boolean;
      --  True if Top refuses to rename From to To.

      function Refused (From, To : String) return Boolean is
      begin
         Top.Rename (From, To);
         return False;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return True;
      end Refused;

   begin
      Ada.Text_IO.Put ("Directory test");

      Top.Open
        (Tests.Registry.Launch
           ("directory",
            (Tests.Registry.Launcher_Cap ("file"),
             Tests.Registry.Launcher_Cap ("directory"))));

      if Top.Writable_Directory_Cap = Rose.Capabilities.Null_Capability then
         return Fail ("no directory server");
      end if;

      --  A file, written through its writable interface.

      declare
         Notes : Rose.Interfaces.Writable_File.Writable_File_Object'Class :=
           Top.Create_File ("notes");
      begin
         if Notes.Writable_File_Cap = Rose.Capabilities.Null_Capability then
            return Fail ("cannot create a file");
         end if;
         Notes.Write (Hello);
         Notes_Cap := Notes.Writable_File_Cap;
      end;

      if Top.Directory_Entry_Count /= 1
        or else Top.Find_Entry ("notes") /= 1
        or else Top.Find_Entry ("other") /= 0
        or else Top.Directory_Entry_Kind (1)
          /= Rose.Interfaces.Directory.Ordinary_File
        or else Top.Directory_Entry_Size (1) /= Hello'Length
      then
         return Fail ("wrong entry for the file");
      end if;

      if Top.Directory_Entry_Name (1) /= "notes" then
         return Fail ("wrong name");
      end if;

      --  Read back through a reader of the read-only file (ADR 0009).

      declare
         Read_Only : Rose.Interfaces.File.File_Object'Class :=
           Top.Get_Ordinary_File (1);
         Reader    : Rose.Interfaces.File_Reader.File_Reader_Object'Class :=
           Read_Only.Open_Reader;
      begin
         Reader.Read (Buffer, Last);
         if Last /= Hello'Length or else Buffer (1 .. Last) /= Hello then
            return Fail ("wrong contents through Get_Ordinary_File");
         end if;
         Reader.Release;
      end;

      --  A subdirectory with a file in it, seen through a read-only
      --  directory.

      declare
         Sub   : Rose.Interfaces.Writable_Directory
           .Writable_Directory_Object'Class :=
             Top.Create_Directory ("sub");
         Inner : Rose.Interfaces.Writable_File.Writable_File_Object'Class :=
           Sub.Create_File ("inner");
      begin
         if Inner.Writable_File_Cap = Rose.Capabilities.Null_Capability then
            return Fail ("cannot create a file in a subdirectory");
         end if;
      end;

      declare
         Sub_Listing : Rose.Interfaces.Directory.Directory_Object'Class :=
           Top.Get_Directory (2);
      begin
         if Sub_Listing.Directory_Entry_Count /= 1
           or else Sub_Listing.Find_Entry ("inner") /= 1
         then
            return Fail ("wrong entries in the subdirectory");
         end if;
      end;

      if Top.Directory_Entry_Kind (2)
        /= Rose.Interfaces.Directory.Directory
      then
         return Fail ("the subdirectory is not a directory");
      end if;

      --  Renaming changes only the entry. The file is the same, so a
      --  cap that was taken for it before still writes to it (#425).

      begin
         Top.Rename ("notes", "memo");
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Fail ("cannot rename");
      end;

      if Top.Directory_Entry_Count /= 2
        or else Top.Find_Entry ("memo") /= 1
        or else Top.Find_Entry ("notes") /= 0
        or else Top.Directory_Entry_Name (1) /= "memo"
      then
         return Fail ("wrong entries after a rename");
      end if;

      begin
         Held.Open (Notes_Cap);
         Held.Write (Hello);
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Fail ("cannot write through a held cap");
      end;
      if Top.Directory_Entry_Size (1) /= 2 * Hello'Length then
         return Fail ("a held cap does not reach the renamed file");
      end if;

      --  A rename fails, and changes nothing, if From is not an entry,
      --  or if To is one already, or is empty.

      if not Refused ("notes", "other")
        or else not Refused ("memo", "sub")
        or else not Refused ("memo", "")
      then
         return Fail ("a bad rename was not refused");
      end if;

      if Top.Find_Entry ("memo") /= 1
        or else Top.Find_Entry ("sub") /= 2
      then
         return Fail ("a refused rename changed the entries");
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
