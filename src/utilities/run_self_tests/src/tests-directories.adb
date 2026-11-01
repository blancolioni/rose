with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;

with Rose.Interfaces.Directory;
with Rose.Interfaces.File;
with Rose.Interfaces.File_Reader;
with Rose.Interfaces.File_Version;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;
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

   subtype File_Version_Class is
     Rose.Interfaces.File_Version.File_Version_Object'Class;
   subtype Writable_File_Class is
     Rose.Interfaces.Writable_File.Writable_File_Object'Class;

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

      function Delete_Refused
        (Name      : String;
         Directory : Boolean)
         return Boolean;
      --  True if Top refuses to delete Name, as a directory if Directory is
      --  True, and as a file if not.

      function Delete_Refused
        (Name      : String;
         Directory : Boolean)
         return Boolean
      is
      begin
         if Directory then
            Top.Delete_Directory (Name);
         else
            Top.Delete_File (Name);
         end if;
         return False;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return True;
      end Delete_Refused;

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

      --  A delete fails, and changes nothing, for a directory as a file,
      --  a file as a directory, a directory that has entries, and a name
      --  that is not an entry (#346).

      if not Delete_Refused ("sub", Directory => False)
        or else not Delete_Refused ("memo", Directory => True)
        or else not Delete_Refused ("sub", Directory => True)
        or else not Delete_Refused ("nothere", Directory => False)
      then
         return Fail ("a bad delete was not refused");
      end if;

      if Top.Directory_Entry_Count /= 2 then
         return Fail ("a refused delete changed the entries");
      end if;

      --  Deleting a file removes the entry and ends its process, so the
      --  cap that is held for it fails.

      begin
         Top.Delete_File ("memo");
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Fail ("cannot delete a file");
      end;

      if Top.Directory_Entry_Count /= 1
        or else Top.Find_Entry ("memo") /= 0
        or else Top.Find_Entry ("sub") /= 1
      then
         return Fail ("wrong entries after deleting a file");
      end if;

      begin
         Held.Write (Hello);
         return Fail ("a deleted file can still be written");
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            null;
      end;

      --  A directory can be deleted once it is empty.

      begin
         declare
            Sub : Rose.Interfaces.Writable_Directory
              .Writable_Directory_Object'Class :=
                Top.Get_Writable_Directory (Top.Find_Entry ("sub"));
         begin
            Sub.Delete_File ("inner");
         end;
         Top.Delete_Directory ("sub");
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Fail ("cannot empty and delete a directory");
      end;

      if Top.Directory_Entry_Count /= 0 then
         return Fail ("wrong entries after deleting a directory");
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

   ------------------
   -- Version_Test --
   ------------------

   function Version_Test return Boolean is
      Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
        Tests.Registry.Launcher ("directory");
      Process  : Rose.Interfaces.Process.Process_Object'Class :=
        Launcher.Launch
          ((Tests.Registry.Launcher_Cap ("file"),
            Tests.Registry.Launcher_Cap ("directory")));
      Top      : Rose.Interfaces.Writable_Directory.Client
        .Writable_Directory_Client;

      function Holds
        (Name     : String;
         Expected : Storage_Array)
         return Boolean;
      --  True if Top has a file Name, which holds Expected.

      function Holds
        (Name     : String;
         Expected : Storage_Array)
         return Boolean
      is
         Index : constant Natural := Top.Find_Entry (Name);
      begin
         if Index = 0 then
            return False;
         end if;

         declare
            File   : Rose.Interfaces.File.File_Object'Class :=
              Top.Get_Ordinary_File (Index);
            Reader : Rose.Interfaces.File_Reader.File_Reader_Object'Class :=
              File.Open_Reader;
            Buffer : Storage_Array (1 .. 16);
            Last   : Storage_Count;
         begin
            Reader.Read (Buffer, Last);
            Reader.Release;
            return Buffer (1 .. Last) = Expected;
         end;
      end Holds;

      function Version_Refused (Name : String) return Boolean;
      --  True if Top refuses to make a version of Name.

      function Version_Refused (Name : String) return Boolean is
      begin
         declare
            Version : File_Version_Class := Top.New_File_Version (Name);
         begin
            Version.Discard;
         end;
         return False;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return True;
      end Version_Refused;

   begin
      Ada.Text_IO.Put ("Version test");

      Top.Open (Process.Published_Interface);
      if Top.Writable_Directory_Cap = Rose.Capabilities.Null_Capability then
         return Fail ("no directory server");
      end if;

      --  A version of a new name is listed only when it is committed.

      declare
         Version : File_Version_Class := Top.New_File_Version ("draft");
         File    : Writable_File_Class := Version.Writable;
      begin
         File.Write (Hello);
         if Top.Find_Entry ("draft") /= 0 then
            return Fail ("a new file is listed before it is committed");
         end if;
         Version.Commit;
      end;

      if not Holds ("draft", Hello) then
         return Fail ("wrong contents of a committed new file");
      end if;

      --  Discarding a version of a new name leaves nothing behind.

      declare
         Version : File_Version_Class := Top.New_File_Version ("scrap");
         File    : Writable_File_Class := Version.Writable;
      begin
         File.Write (Hello);
         Version.Discard;
      end;

      if Top.Find_Entry ("scrap") /= 0
        or else Top.Directory_Entry_Count /= 1
      then
         return Fail ("a discarded new file is listed");
      end if;

      --  A version of a file starts as a copy of it, and the file is
      --  unchanged until the version is committed. A file has one
      --  version at a time.

      declare
         Version : File_Version_Class := Top.New_File_Version ("draft");
         File    : Writable_File_Class := Version.Writable;
      begin
         if File.Size /= Hello'Length then
            return Fail ("a version is not a copy of its file");
         end if;

         File.Truncate (0);
         File.Write (Hello (3 .. 5));
         if not Holds ("draft", Hello) then
            return Fail ("writing a version changed its file");
         end if;

         if not Version_Refused ("draft") then
            return Fail ("a file has two versions");
         end if;

         Version.Commit;
      end;

      if not Holds ("draft", Hello (3 .. 5)) then
         return Fail ("wrong contents after committing a version");
      end if;

      --  Discarding a version leaves the file as it was.

      declare
         Version : File_Version_Class := Top.New_File_Version ("draft");
         File    : Writable_File_Class := Version.Writable;
      begin
         File.Write (Hello);
         Version.Discard;
      end;

      if not Holds ("draft", Hello (3 .. 5)) then
         return Fail ("discarding a version changed its file");
      end if;

      --  A new name that was taken in the meantime can't be committed,
      --  and the version can then be discarded.

      declare
         Version : File_Version_Class := Top.New_File_Version ("late");
         Taken   : Writable_File_Class := Top.Create_File ("late");
      begin
         if Taken.Writable_File_Cap = Rose.Capabilities.Null_Capability then
            return Fail ("cannot create a file");
         end if;

         begin
            Version.Commit;
            return Fail ("a version was committed over a new entry");
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;
         Version.Discard;
      end;

      --  A version of a file that was deleted can't be committed.

      declare
         Version : File_Version_Class := Top.New_File_Version ("draft");
      begin
         Top.Delete_File ("draft");
         begin
            Version.Commit;
            return Fail ("a version of a deleted file was committed");
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;
         Version.Discard;
      end;

      --  No version is made of a directory, or of an empty name.

      declare
         Sub : Rose.Interfaces.Writable_Directory
           .Writable_Directory_Object'Class :=
             Top.Create_Directory ("sub");
      begin
         if Sub.Writable_Directory_Cap = Rose.Capabilities.Null_Capability
         then
            return Fail ("cannot create a directory");
         end if;
      end;

      if not Version_Refused ("sub") or else not Version_Refused ("") then
         return Fail ("a bad version was not refused");
      end if;

      --  The directory and its files end with the test.

      Top.Delete_File ("late");
      Top.Delete_Directory ("sub");
      Process.Destroy;
      return True;
   end Version_Test;

end Tests.Directories;
