with System.Storage_Elements;

with Rose.Capabilities;

with Rose.Interfaces.Common.Tree;
with Rose.Interfaces.Files.Directory;
with Rose.Interfaces.Files.File;
with Rose.Interfaces.Files.Fixed_Directory;
with Rose.Interfaces.Files.Transactional_Writable_File;
with Rose.Interfaces.Files.Writable_Directory;
with Rose.Interfaces.Files.Writable_File;
with Rose.Interfaces.Streams.Readable;
with Rose.Interfaces.Streams.Reader;
with Rose.Interfaces.Streams.Releasable_Reader;
with Rose.Interfaces.Streams.Releasable_Writer;
with Rose.Interfaces.Streams.Seekable_Reader;
with Rose.Interfaces.Streams.Stream;

private with Ada.Finalization;
private with Ada.Strings.Unbounded;

package Rose.Host.Files is

   --  Directories and files over the host's. A directory's entries are
   --  in the order of their names. A special file, such as a FIFO, is
   --  listed as one, but can't be opened. An entry is a name, never a
   --  path, so a name with a / in it can't be one.

   type Directory is limited new
     Rose.Interfaces.Files.Writable_Directory.Writable_Directory_Object
     with private;
   --  A directory, and anything under it, can be changed: the host's
   --  permissions are what stop a change.

   procedure Open
     (Item : in out Directory;
      Name : String);
   --  The backing of a directory argument: the current directory if Name
   --  is "", which is an argument that wasn't given, and otherwise the
   --  directory Name. Raises Command_Failed if there is none.

   type File is limited new
     Rose.Interfaces.Files.Transactional_Writable_File
       .Transactional_Writable_File_Object
     with private;
   --  A file, which is written in place. Commit and Rollback do nothing,
   --  and New_Version gives a Version of it.

   procedure Open
     (Item : in out File;
      Name : String);
   --  The backing of a File or Writable_File argument: the file Name,
   --  which must be there. Raises Command_Failed if it isn't.

   --  The operations of the interfaces. A call that fails raises
   --  Rose.Interfaces.Objects.Invocation_Error.

   overriding function Tree_Cap
     (Item : in out Directory)
      return Rose.Capabilities.Capability;

   overriding function Entry_Count (Item : in out Directory) return Natural;

   overriding function Entry_Name
     (Item  : in out Directory;
      Index : Positive)
      return String;

   overriding function Entry_Kind_Of
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Common.Tree.Entry_Kind;

   overriding function Find_Entry
     (Item : in out Directory;
      Name : String)
      return Natural;

   overriding function Get_Branch
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Common.Tree.Tree_Object'Class;

   overriding function Get_Leaf
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Readable.Readable_Object'Class;

   overriding function Directory_Cap
     (Item : in out Directory)
      return Rose.Capabilities.Capability;

   overriding function Entry_File_Kind
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Directory.File_Kind;

   overriding function Entry_Size
     (Item  : in out Directory;
      Index : Positive)
      return System.Storage_Elements.Storage_Count;

   overriding function Get_Ordinary_File
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Files.File.File_Object'Class;

   overriding function Get_Directory
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Directory.Directory_Object'Class;

   overriding function Get_Special_Reader
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Reader.Reader_Object'Class;

   overriding function Fixed_Directory_Cap
     (Item : in out Directory)
      return Rose.Capabilities.Capability;

   overriding function Get_Writable_File
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Writable_File.Writable_File_Object'Class;

   overriding function Get_Fixed_Directory
     (Item  : in out Directory;
      Index : Positive)
      return
        Rose.Interfaces.Files.Fixed_Directory.Fixed_Directory_Object'Class;

   overriding function Get_Special_File
     (Item  : in out Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Stream.Stream_Object'Class;

   overriding function Writable_Directory_Cap
     (Item : in out Directory)
      return Rose.Capabilities.Capability;

   overriding function Get_Writable_Directory
     (Item  : in out Directory;
      Index : Positive)
      return
        Rose.Interfaces.Files.Writable_Directory
          .Writable_Directory_Object'Class;

   overriding function Create_File
     (Item : in out Directory;
      Name : String)
      return Rose.Interfaces.Files.Writable_File.Writable_File_Object'Class;
   --  Fails if Name is already an entry.

   overriding function Create_Directory
     (Item : in out Directory;
      Name : String)
      return
        Rose.Interfaces.Files.Writable_Directory
          .Writable_Directory_Object'Class;
   --  Fails if Name is already an entry.

   overriding procedure Rename
     (Item : in out Directory;
      From : String;
      To   : String);
   --  Fails if From isn't an entry, or To is one.

   overriding procedure Delete_File
     (Item : in out Directory;
      Name : String);
   --  Fails if Name isn't a file.

   overriding procedure Delete_Directory
     (Item : in out Directory;
      Name : String);
   --  Fails if Name isn't an empty directory.

   overriding procedure Add_Special_File
     (Item : in out Directory;
      Name : String;
      File : in out Rose.Interfaces.Streams.Stream.Stream_Object'Class);
   --  Always fails: the host has no special files.

   overriding function Readable_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability;

   overriding function Open_Reader
     (Item : in out File)
      return
        Rose.Interfaces.Streams.Releasable_Reader
          .Releasable_Reader_Object'Class;

   overriding function File_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability;

   overriding function Size
     (Item : in out File)
      return System.Storage_Elements.Storage_Count;

   overriding function Open_Seekable_Reader
     (Item : in out File)
      return
        Rose.Interfaces.Streams.Seekable_Reader.Seekable_Reader_Object'Class;

   overriding function Writable_File_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability;

   overriding function Append
     (Item : in out File)
      return
        Rose.Interfaces.Streams.Releasable_Writer
          .Releasable_Writer_Object'Class;

   overriding function Rewrite
     (Item : in out File)
      return
        Rose.Interfaces.Streams.Releasable_Writer
          .Releasable_Writer_Object'Class;

   overriding function New_Version
     (Item : in out File)
      return
        Rose.Interfaces.Files.Transactional_Writable_File
          .Transactional_Writable_File_Object'Class;

   overriding function Transactional_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability;

   overriding procedure Commit (Item : in out File);

   overriding procedure Rollback (Item : in out File);

   overriding function Resource_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability;

   overriding procedure Release (Item : in out File);

   overriding function Transactional_Writable_File_Cap
     (Item : in out File)
      return Rose.Capabilities.Capability;

   type Version is new File with private;
   --  A version of a file (ADR 0026): a copy of it, written beside it,
   --  which Commit puts in its place. A version that is released, rolled
   --  back or finalized before it is committed is deleted, and the file is
   --  as it was.

   overriding procedure Open
     (Item : in out Version;
      Name : String);
   --  The backing of a Transactional_Writable_File argument: a version of
   --  the file Name, which is made, empty, if there is none, as petal
   --  makes it. Raises Command_Failed if it can't be.

private

   use Ada.Strings.Unbounded;

   type Directory is limited new
     Rose.Interfaces.Files.Writable_Directory.Writable_Directory_Object with
      record
         Path : Unbounded_String;
      end record;

   type Version_Cleanup (Owner : not null access File'Class) is
     new Ada.Finalization.Limited_Controlled with null record;
   --  Deletes Owner's version when Owner goes, unless it was committed.

   overriding procedure Finalize (Item : in out Version_Cleanup);

   type File is limited new
     Rose.Interfaces.Files.Transactional_Writable_File
       .Transactional_Writable_File_Object with
      record
         Path         : Unbounded_String;
         --  The file.
         Version_Path : Unbounded_String;
         --  For a version, where it is written; otherwise "".
         Cleanup      : Version_Cleanup (File'Access);
      end record;

   type Version is new File with null record;

end Rose.Host.Files;
