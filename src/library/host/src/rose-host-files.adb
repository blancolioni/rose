with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;

with Rose.Command_Line;
with Rose.Interfaces.Objects;

with Rose.Host.Streams;

package body Rose.Host.Files is

   use all type Ada.Directories.File_Kind;

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Sorting is new Name_Vectors.Generic_Sorting;

   Version_Suffix : constant String := ".rose-version";
   --  A version of a file is written beside it, with this added to its
   --  name, so that Commit is a rename.

   procedure Fail (Message : String)
     with No_Return;
   --  Fail the call: raise Invocation_Error with Message.

   function Entries (Item : Directory) return Name_Vectors.Vector;
   --  The names of Item's entries, in order.

   function Entry_Path
     (Item  : Directory;
      Index : Positive)
      return String;
   --  The path of Item's entry Index. Fails if there is none.

   function Child_Path
     (Item : Directory;
      Name : String)
      return String;
   --  The path of the entry Name of Item, which may not be there. Fails
   --  if Name can't be an entry's name.

   function Kind_Of (Path : String) return Ada.Directories.File_Kind
   is (Ada.Directories.Kind (Path));

   function Current (Item : File'Class) return String
   is (if Item.Version_Path = Null_Unbounded_String
       then To_String (Item.Path)
       else To_String (Item.Version_Path));
   --  Where Item is read and written: its version, if it is one.

   procedure Start_Version
     (Item : in out File'Class;
      Path : String);
   --  Make Item a version of the file Path, a copy of it beside it.

   procedure Discard_Version (Item : in out File'Class);
   --  Delete Item's version, if it has one that isn't committed.

   ----------------------
   -- Add_Special_File --
   ----------------------

   overriding procedure Add_Special_File
     (Item : in out Directory;
      Name : String;
      File : in out Rose.Interfaces.Streams.Stream.Stream_Object'Class)
   is
   begin
      Fail ("the host has no special files to add");
   end Add_Special_File;

   ------------
   -- Append --
   ------------

   overriding function Append
     (Item : in out File)
      return
        Rose.Interfaces.Streams.Releasable_Writer
          .Releasable_Writer_Object'Class
   is
   begin
      return Writer : Rose.Host.Streams.Writer do
         Rose.Host.Streams.Open_Path (Writer, Current (Item), Append => True);
      end return;
   end Append;

   ----------------
   -- Child_Path --
   ----------------

   function Child_Path
     (Item : Directory;
      Name : String)
      return String
   is
   begin
      if Name = "" or else Name = "." or else Name = ".."
        or else (for some Ch of Name => Ch = '/' or else Ch = '\')
      then
         Fail ("'" & Name & "' is not the name of an entry");
      end if;
      return Ada.Directories.Compose (To_String (Item.Path), Name);
   end Child_Path;

   ------------
   -- Commit --
   ------------

   overriding procedure Commit (Item : in out File) is
      Path : constant String := To_String (Item.Path);
   begin
      if Item.Version_Path = Null_Unbounded_String then
         return;
      end if;

      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
      Ada.Directories.Rename (To_String (Item.Version_Path), Path);
      Item.Version_Path := Null_Unbounded_String;
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Fail ("cannot commit " & Path);
   end Commit;

   ----------------------
   -- Create_Directory --
   ----------------------

   overriding function Create_Directory
     (Item : in out Directory;
      Name : String)
      return
        Rose.Interfaces.Files.Writable_Directory
          .Writable_Directory_Object'Class
   is
      Path : constant String := Child_Path (Item, Name);
   begin
      if Ada.Directories.Exists (Path) then
         Fail (Name & " is already an entry");
      end if;

      begin
         Ada.Directories.Create_Directory (Path);
      exception
         when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
            Fail ("cannot create directory " & Path);
      end;

      return Result : Directory do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   overriding function Create_File
     (Item : in out Directory;
      Name : String)
      return Rose.Interfaces.Files.Writable_File.Writable_File_Object'Class
   is
      Path : constant String := Child_Path (Item, Name);
   begin
      if Ada.Directories.Exists (Path) then
         Fail (Name & " is already an entry");
      end if;

      declare
         Created : Ada.Streams.Stream_IO.File_Type;
      begin
         Ada.Streams.Stream_IO.Create
           (Created, Ada.Streams.Stream_IO.Out_File, Path);
         Ada.Streams.Stream_IO.Close (Created);
      exception
         when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
            Fail ("cannot create " & Path);
      end;

      return Result : File do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Create_File;

   ----------------------
   -- Delete_Directory --
   ----------------------

   overriding procedure Delete_Directory
     (Item : in out Directory;
      Name : String)
   is
      Path : constant String := Child_Path (Item, Name);
   begin
      if not Ada.Directories.Exists (Path)
        or else Kind_Of (Path) /= Ada.Directories.Directory
      then
         Fail (Name & " is not a directory");
      end if;
      Ada.Directories.Delete_Directory (Path);
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Fail ("cannot delete directory " & Path);
   end Delete_Directory;

   -----------------
   -- Delete_File --
   -----------------

   overriding procedure Delete_File
     (Item : in out Directory;
      Name : String)
   is
      Path : constant String := Child_Path (Item, Name);
   begin
      if not Ada.Directories.Exists (Path)
        or else Kind_Of (Path) /= Ordinary_File
      then
         Fail (Name & " is not a file");
      end if;
      Ada.Directories.Delete_File (Path);
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Fail ("cannot delete " & Path);
   end Delete_File;

   -------------------
   -- Directory_Cap --
   -------------------

   overriding function Directory_Cap
     (Item : in out Directory)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   ---------------------
   -- Discard_Version --
   ---------------------

   procedure Discard_Version (Item : in out File'Class) is
      Path : constant String := To_String (Item.Version_Path);
   begin
      Item.Version_Path := Null_Unbounded_String;
      if Path /= "" and then Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         null;
   end Discard_Version;

   -------------
   -- Entries --
   -------------

   function Entries (Item : Directory) return Name_Vectors.Vector is
      Search : Ada.Directories.Search_Type;
      Found  : Ada.Directories.Directory_Entry_Type;
   begin
      return Result : Name_Vectors.Vector do
         Ada.Directories.Start_Search (Search, To_String (Item.Path), "");
         while Ada.Directories.More_Entries (Search) loop
            Ada.Directories.Get_Next_Entry (Search, Found);
            declare
               Name : constant String :=
                 Ada.Directories.Simple_Name (Found);
            begin
               if Name /= "." and then Name /= ".." then
                  Result.Append (Name);
               end if;
            end;
         end loop;
         Ada.Directories.End_Search (Search);
         Sorting.Sort (Result);
      end return;
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Fail ("cannot read directory " & To_String (Item.Path));
   end Entries;

   -----------------
   -- Entry_Count --
   -----------------

   overriding function Entry_Count (Item : in out Directory) return Natural
   is (Natural (Entries (Item).Length));

   ---------------------
   -- Entry_File_Kind --
   ---------------------

   overriding function Entry_File_Kind
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Directory.File_Kind
   is
   begin
      case Kind_Of (Entry_Path (Item, Index)) is
         when Ada.Directories.Directory =>
            return Rose.Interfaces.Files.Directory.Directory;
         when Ordinary_File =>
            return Rose.Interfaces.Files.Directory.Ordinary_File;
         when Special_File =>
            return Rose.Interfaces.Files.Directory.Special_File;
      end case;
   end Entry_File_Kind;

   -------------------
   -- Entry_Kind_Of --
   -------------------

   overriding function Entry_Kind_Of
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Common.Tree.Entry_Kind
   is
   begin
      case Kind_Of (Entry_Path (Item, Index)) is
         when Ada.Directories.Directory =>
            return Rose.Interfaces.Common.Tree.Branch;
         when Ordinary_File =>
            return Rose.Interfaces.Common.Tree.Leaf;
         when Special_File =>
            return Rose.Interfaces.Common.Tree.Other;
      end case;
   end Entry_Kind_Of;

   ----------------
   -- Entry_Name --
   ----------------

   overriding function Entry_Name
     (Item  : in out Directory;
      Index : Positive)
      return String
   is
      Names : constant Name_Vectors.Vector := Entries (Item);
   begin
      if Index > Names.Last_Index then
         Fail ("there is no entry" & Index'Image);
      end if;
      return Names (Index);
   end Entry_Name;

   ----------------
   -- Entry_Path --
   ----------------

   function Entry_Path
     (Item  : Directory;
      Index : Positive)
      return String
   is
      Names : constant Name_Vectors.Vector := Entries (Item);
   begin
      if Index > Names.Last_Index then
         Fail ("there is no entry" & Index'Image);
      end if;
      return Ada.Directories.Compose (To_String (Item.Path), Names (Index));
   end Entry_Path;

   ----------------
   -- Entry_Size --
   ----------------

   overriding function Entry_Size
     (Item  : in out Directory;
      Index : Positive)
      return System.Storage_Elements.Storage_Count
   is
      Path : constant String := Entry_Path (Item, Index);
   begin
      if Kind_Of (Path) = Ordinary_File then
         return System.Storage_Elements.Storage_Count
           (Ada.Directories.Size (Path));
      else
         return 0;
      end if;
   end Entry_Size;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String) is
   begin
      raise Rose.Interfaces.Objects.Invocation_Error with Message;
   end Fail;

   --------------
   -- File_Cap --
   --------------

   overriding function File_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item : in out Version_Cleanup) is
   begin
      Discard_Version (Item.Owner.all);
   end Finalize;

   ----------------
   -- Find_Entry --
   ----------------

   overriding function Find_Entry
     (Item : in out Directory;
      Name : String)
      return Natural
   is
      Index : constant Natural := Entries (Item).Find_Index (Name);
   begin
      return (if Index = Name_Vectors.No_Index then 0 else Index);
   end Find_Entry;

   -------------------------
   -- Fixed_Directory_Cap --
   -------------------------

   overriding function Fixed_Directory_Cap
     (Item : in out Directory)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   ----------------
   -- Get_Branch --
   ----------------

   overriding function Get_Branch
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Common.Tree.Tree_Object'Class
   is
      Path : constant String := Entry_Path (Item, Index);
   begin
      if Kind_Of (Path) /= Ada.Directories.Directory then
         Fail (Path & " is not a directory");
      end if;
      return Result : Directory do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Get_Branch;

   -------------------
   -- Get_Directory --
   -------------------

   overriding function Get_Directory
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Directory.Directory_Object'Class
   is
      Path : constant String := Entry_Path (Item, Index);
   begin
      if Kind_Of (Path) /= Ada.Directories.Directory then
         Fail (Path & " is not a directory");
      end if;
      return Result : Directory do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Get_Directory;

   -------------------------
   -- Get_Fixed_Directory --
   -------------------------

   overriding function Get_Fixed_Directory
     (Item  : in out Directory;
      Index : Positive)
      return
        Rose.Interfaces.Files.Fixed_Directory.Fixed_Directory_Object'Class
   is
      Path : constant String := Entry_Path (Item, Index);
   begin
      if Kind_Of (Path) /= Ada.Directories.Directory then
         Fail (Path & " is not a directory");
      end if;
      return Result : Directory do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Get_Fixed_Directory;

   --------------
   -- Get_Leaf --
   --------------

   overriding function Get_Leaf
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Readable.Readable_Object'Class
   is
      Path : constant String := Entry_Path (Item, Index);
   begin
      if Kind_Of (Path) /= Ordinary_File then
         Fail (Path & " is not a file");
      end if;
      return Result : File do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Get_Leaf;

   -----------------------
   -- Get_Ordinary_File --
   -----------------------

   overriding function Get_Ordinary_File
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Files.File.File_Object'Class
   is
      Path : constant String := Entry_Path (Item, Index);
   begin
      if Kind_Of (Path) /= Ordinary_File then
         Fail (Path & " is not a file");
      end if;
      return Result : File do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Get_Ordinary_File;

   ----------------------
   -- Get_Special_File --
   ----------------------

   overriding function Get_Special_File
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Stream.Stream_Object'Class
   is (raise Rose.Interfaces.Objects.Invocation_Error
         with "the host's special files can't be opened");

   ------------------------
   -- Get_Special_Reader --
   ------------------------

   overriding function Get_Special_Reader
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Reader.Reader_Object'Class
   is (raise Rose.Interfaces.Objects.Invocation_Error
         with "the host's special files can't be opened");

   ----------------------------
   -- Get_Writable_Directory --
   ----------------------------

   overriding function Get_Writable_Directory
     (Item  : in out Directory;
      Index : Positive)
      return
        Rose.Interfaces.Files.Writable_Directory
          .Writable_Directory_Object'Class
   is
      Path : constant String := Entry_Path (Item, Index);
   begin
      if Kind_Of (Path) /= Ada.Directories.Directory then
         Fail (Path & " is not a directory");
      end if;
      return Result : Directory do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Get_Writable_Directory;

   -----------------------
   -- Get_Writable_File --
   -----------------------

   overriding function Get_Writable_File
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Writable_File.Writable_File_Object'Class
   is
      Path : constant String := Entry_Path (Item, Index);
   begin
      if Kind_Of (Path) /= Ordinary_File then
         Fail (Path & " is not a file");
      end if;
      return Result : File do
         Result.Path := To_Unbounded_String (Path);
      end return;
   end Get_Writable_File;

   -----------------
   -- New_Version --
   -----------------

   overriding function New_Version
     (Item : in out File)
      return
        Rose.Interfaces.Files.Transactional_Writable_File
          .Transactional_Writable_File_Object'Class
   is
   begin
      return Result : Version do
         Start_Version (Result, To_String (Item.Path));
      end return;
   end New_Version;

   ----------
   -- Open --
   ----------

   procedure Open
     (Item : in out Directory;
      Name : String)
   is
   begin
      if Name = "" then
         Item.Path := To_Unbounded_String (Ada.Directories.Current_Directory);
      elsif Ada.Directories.Exists (Name)
        and then Kind_Of (Name) = Ada.Directories.Directory
      then
         Item.Path := To_Unbounded_String (Name);
      else
         raise Rose.Command_Line.Command_Failed
           with "no directory named '" & Name & "'";
      end if;
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (Item : in out File;
      Name : String)
   is
   begin
      if not Ada.Directories.Exists (Name) then
         raise Rose.Command_Line.Command_Failed
           with "no file named '" & Name & "'";
      elsif Kind_Of (Name) /= Ordinary_File then
         raise Rose.Command_Line.Command_Failed
           with "'" & Name & "' is not a file";
      end if;
      Item.Path := To_Unbounded_String (Name);
   end Open;

   ----------
   -- Open --
   ----------

   overriding procedure Open
     (Item : in out Version;
      Name : String)
   is
   begin
      if not Ada.Directories.Exists (Name) then
         declare
            Created : Ada.Streams.Stream_IO.File_Type;
         begin
            Ada.Streams.Stream_IO.Create
              (Created, Ada.Streams.Stream_IO.Out_File, Name);
            Ada.Streams.Stream_IO.Close (Created);
         end;
      elsif Kind_Of (Name) /= Ordinary_File then
         raise Rose.Command_Line.Command_Failed
           with "'" & Name & "' is not a file";
      end if;
      Start_Version (Item, Name);
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error
         | Rose.Interfaces.Objects.Invocation_Error =>
         raise Rose.Command_Line.Command_Failed
           with "cannot write '" & Name & "'";
   end Open;

   -----------------
   -- Open_Reader --
   -----------------

   overriding function Open_Reader
     (Item : in out File)
      return
        Rose.Interfaces.Streams.Releasable_Reader
          .Releasable_Reader_Object'Class
   is
   begin
      return Reader : Rose.Host.Streams.Reader do
         Rose.Host.Streams.Open_Path (Reader, Current (Item));
      end return;
   end Open_Reader;

   --------------------------
   -- Open_Seekable_Reader --
   --------------------------

   overriding function Open_Seekable_Reader
     (Item : in out File)
      return
        Rose.Interfaces.Streams.Seekable_Reader.Seekable_Reader_Object'Class
   is
   begin
      return Reader : Rose.Host.Streams.Reader do
         Rose.Host.Streams.Open_Path (Reader, Current (Item));
      end return;
   end Open_Seekable_Reader;

   ------------------
   -- Readable_Cap --
   ------------------

   overriding function Readable_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out File) is
   begin
      Discard_Version (Item);
   end Release;

   ------------
   -- Rename --
   ------------

   overriding procedure Rename
     (Item : in out Directory;
      From : String;
      To   : String)
   is
      From_Path : constant String := Child_Path (Item, From);
      To_Path   : constant String := Child_Path (Item, To);
   begin
      if not Ada.Directories.Exists (From_Path) then
         Fail (From & " is not an entry");
      elsif Ada.Directories.Exists (To_Path) then
         Fail (To & " is already an entry");
      end if;
      Ada.Directories.Rename (From_Path, To_Path);
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Fail ("cannot rename " & From_Path);
   end Rename;

   ------------------
   -- Resource_Cap --
   ------------------

   overriding function Resource_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   -------------
   -- Rewrite --
   -------------

   overriding function Rewrite
     (Item : in out File)
      return
        Rose.Interfaces.Streams.Releasable_Writer
          .Releasable_Writer_Object'Class
   is
   begin
      return Writer : Rose.Host.Streams.Writer do
         Rose.Host.Streams.Open_Path
           (Writer, Current (Item), Append => False);
      end return;
   end Rewrite;

   --------------
   -- Rollback --
   --------------

   overriding procedure Rollback (Item : in out File) is
   begin
      Discard_Version (Item);
   end Rollback;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Item : in out File)
      return System.Storage_Elements.Storage_Count
   is
   begin
      return System.Storage_Elements.Storage_Count
        (Ada.Directories.Size (Current (Item)));
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Fail ("cannot read " & Current (Item));
   end Size;

   -------------------
   -- Start_Version --
   -------------------

   procedure Start_Version
     (Item : in out File'Class;
      Path : String)
   is
      Copy : constant String := Path & Version_Suffix;
   begin
      Item.Path := To_Unbounded_String (Path);
      if Ada.Directories.Exists (Copy) then
         Ada.Directories.Delete_File (Copy);
      end if;
      Ada.Directories.Copy_File (Path, Copy);
      Item.Version_Path := To_Unbounded_String (Copy);
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Fail ("cannot make a version of " & Path);
   end Start_Version;

   --------------
   -- Tree_Cap --
   --------------

   overriding function Tree_Cap
     (Item : in out Directory)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   -----------------------
   -- Transactional_Cap --
   -----------------------

   overriding function Transactional_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   -------------------------------------
   -- Transactional_Writable_File_Cap --
   -------------------------------------

   overriding function Transactional_Writable_File_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   ----------------------------
   -- Writable_Directory_Cap --
   ----------------------------

   overriding function Writable_Directory_Cap
     (Item : in out Directory)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

   -----------------------
   -- Writable_File_Cap --
   -----------------------

   overriding function Writable_File_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Null_Capability);

end Rose.Host.Files;
