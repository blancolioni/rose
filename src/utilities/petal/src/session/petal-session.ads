with Rose.Capabilities;

with Rose.Interfaces.Users.User;

package Petal.Session is

   --  The user and the directories that petal is given (ADR 0003): the
   --  user's User interface, and a table of named directories whose
   --  first entry is the user's home directory, home (ADR 0002,
   --  section 5), and which also has system (ADR 0035, ADR 0044).

   --  The current directory (#342) is one of the named directories, its
   --  root, and a path from it. A directory doesn't know its parent, so
   --  petal keeps the directories on the path. It starts as home.

   --  Every path petal reads is a URI reference (ADR 0044): //name/path
   --  starts from the named directory name, /path from the root of the
   --  current directory, and any other path from the current directory.
   --  "." is the directory itself, and ".." the one before it on the
   --  path. ".." above a named directory, an empty name between two "/",
   --  a name that isn't in the table, and a ":" before the first "/" (a
   --  scheme, which petal doesn't know) are errors, which Path_Error
   --  describes.

   procedure Start (User : in out Rose.Interfaces.Users.User
     .User_Object'Class);
   --  Start the session of User, with User's home directory as home. A
   --  user with no home directory has no current directory.

   procedure Run_Startup_File;
   --  Run each line of .petalrc in the home directory as if it had been
   --  typed, but without echoing it or showing a prompt. Do nothing if
   --  there is no home directory, or no .petalrc in it.

   procedure Set_System (Directory : Rose.Capabilities.Capability);
   --  Name Directory, the cap of a Fixed_Directory interface, system in
   --  the table (ADR 0035, section 2; ADR 0036, section 3; ADR 0044,
   --  section 3), unless it is null. Nothing in it can be added, renamed
   --  or deleted, and a command gets it, or a directory in it, only as a
   --  read-only Directory.

   procedure Add_Directory
     (Name      : String;
      Directory : Rose.Capabilities.Capability);
   --  Name Directory, the cap of a Writable_Directory interface, in the
   --  table, or report that the table is full. The table keeps the cap.

   function Package_State
     (Package_Name : String)
      return Rose.Capabilities.Capability;
   --  The Writable_Directory of home/package-state/<package> (ADR 0040),
   --  where <package> is Package_Name in lower case with "-" for ".",
   --  made if it isn't there, or the null cap if there is no home or it
   --  can't be made. Release_Files deletes the cap.

   function Path_Error return String;
   --  Why the last path that couldn't be resolved couldn't be.

   procedure Find_Grant
     (Path     : String;
      Writable : Boolean;
      Grant    : out Rose.Capabilities.Capability;
      Found    : out Boolean);
   --  A grant of the directory at Path (ADR 0044, section 2). For an in
   --  Directory argument it is read-only: the interface cap of its
   --  Directory interface, which has only the lookup operations (ADR
   --  0003, section 1). If Writable, for an in Writable_Directory
   --  argument, it is the cap of the directory's Writable_Directory
   --  interface, so the command's changes take effect as it makes them
   --  (ADR 0012, section 3), and there is none in system. A grant of a
   --  named directory itself is petal's own, and kept. A grant of a
   --  directory below one is petal's until Release_Files deletes it.
   --  Found is False, with Path_Error saying why, if there is no grant.

   type File_Lookup is (File_Found, No_Such_File, Not_A_File, Bad_Path);
   --  Bad_Path: Path_Error says why the path couldn't be resolved.

   procedure Find_File
     (Path     : String;
      Writable : Boolean;
      File     : out Rose.Capabilities.Capability;
      Result   : out File_Lookup);
   --  For an in File argument (#344): the interface cap of the read-only
   --  File interface of the ordinary file at Path. If Writable, for an in
   --  Writable_File argument, it is the file's Writable_File interface,
   --  so the command's changes take effect as it makes them (ADR 0026).
   --  Result says whether there is such a file. The cap is petal's until
   --  Release_Files deletes it.

   procedure Open_Reader
     (Path   : String;
      Reader : out Rose.Capabilities.Capability;
      Result : out File_Lookup);
   --  For an in Reader argument whose default is standard input (#395):
   --  a reader of its own on the ordinary file at Path, from the start of
   --  the file (ADR 0009), or the reader of the special file at Path (ADR
   --  0035, section 5). Reader is the reader's Read cap, which the
   --  command reads as it reads standard input. Result is as for
   --  Find_File. The reader is petal's until Release_Files releases it.

   procedure Release_Files;
   --  Delete the caps that Find_File and Find_Grant have returned since
   --  the last call, and release the readers that Open_Reader has
   --  opened, once the command they were bound for has run.

   type Version_Lookup is
     (Version_Made, Not_A_Version_File, No_Version, Bad_Path);
   --  Bad_Path: Path_Error says why the path couldn't be resolved.

   procedure New_Version
     (Path   : String;
      File   : out Rose.Capabilities.Capability;
      Result : out Version_Lookup);
   --  For an in Transactional_Writable_File argument (ADR 0026): a
   --  version of the file at Path, from the file's own New_Version, which
   --  the command commits. If there is no such entry, the file is created
   --  first, so a command that fails leaves it empty (ADR 0027). The
   --  version starts as a copy of the file, and the command empties it if
   --  it replaces the file (ADR 0009, section 4). File is the interface
   --  cap of the version. Result says whether the version was made: not
   --  if Path is not a file, or the directory or the file refuses. The
   --  version is petal's until Release_Versions.

   procedure Release_Versions;
   --  Release the versions that New_Version has made since the last call,
   --  once the command they were bound for has ended, whether it
   --  completed or failed. What the command didn't commit is dropped.

   procedure Redirect_Output
     (Path   : String;
      Append : Boolean;
      Output : out Rose.Capabilities.Capability;
      Result : out Version_Lookup);
   --  For "> Path" and ">> Path" (#338): a version of the file at Path,
   --  made as New_Version makes one, and a writer on it: Rewrite, which
   --  empties the version, or Append if Append (ADR 0009, section 4).
   --  Output is the writer's Write cap, for the command's standard
   --  output. The command doesn't know about the file, so the version is
   --  petal's to commit (ADR 0026), in End_Redirection. If Path is a
   --  special file, Output is its writer, and nothing is committed (ADR
   --  0035, section 5). Result is as for New_Version.

   procedure End_Redirection (Completed : Boolean);
   --  The command whose output Redirect_Output redirected has ended:
   --  release the writer, commit the version if the command Completed,
   --  and release the version, which drops what wasn't committed. A
   --  failed command leaves the file as it was. Do nothing if there is no
   --  redirection.

   procedure Current_Grant
     (Writable : Boolean;
      Grant    : out Rose.Capabilities.Capability;
      Found    : out Boolean);
   --  A grant of the current directory, as Find_Grant gives, for an in
   --  Directory argument that is not given. Found is False if there is
   --  no current directory, or Writable and it is in system.

   procedure Change_Directory (Path : String);
   --  Make the directory at Path the current directory. If there is no
   --  directory at Path, report why, and leave the current directory as
   --  it was.

   function Current_Directory_Name return String;
   --  The current directory as pwd writes it (#343), as a path that
   --  petal accepts anywhere: "//", the name of its root, then the path
   --  from the root, with "/" before each name.

   Home_Name : constant String := "home";
   --  The user's home directory, the first entry in the table, and the
   --  first current directory.

   function User_Cap return Rose.Capabilities.Capability;
   --  The User interface cap of the user petal was started for, or null
   --  if there is none. It is the session's: don't close a client of it.

end Petal.Session;
