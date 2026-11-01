with Rose.Capabilities;

with Rose.Interfaces.User.Client;
with Rose.Interfaces.Writable_Directory.Client;

package Petal.Session is

   --  The user and the directories that petal is given (ADR 0003): the
   --  user's User interface, and a table of named directories whose
   --  first entry is the user's home directory, home (ADR 0002,
   --  section 5).

   procedure Start (User : Rose.Interfaces.User.Client.User_Client);
   --  Start the session of User, with User's home directory as home.
   --  Take the user and the home directory from petal's argument cap
   --  set. Either can be missing, as when petal is not started for a
   --  user.

   procedure Run_Startup_File;
   --  Run each line of .petalrc in the home directory as if it had been
   --  typed, but without echoing it or showing a prompt. Do nothing if
   --  there is no home directory, or no .petalrc in it.

   procedure Add_Directory
     (Name      : String;
      Directory : Rose.Interfaces.Writable_Directory.Client
                    .Writable_Directory_Client);
   --  Name Directory in the table, or report that the table is full.

   procedure Find_Directory
     (Name      : String;
      Directory : out Rose.Interfaces.Writable_Directory.Client
                        .Writable_Directory_Client;
      Found     : out Boolean);
   --  The directory called Name in the table, if there is one.

   procedure Find_Grant
     (Name     : String;
      Writable : Boolean;
      Grant    : out Rose.Capabilities.Capability;
      Found    : out Boolean);
   --  A grant of the directory called Name. For an in Directory argument
   --  it is read-only: the interface cap of its Directory interface,
   --  which has only the lookup operations (ADR 0003, section 1), made
   --  the first time it is asked for and then kept. If Writable, for an
   --  in Writable_Directory argument, it is the cap of petal's own
   --  Writable_Directory interface, so the command's changes take
   --  effect as it makes them (ADR 0012, section 3).

   type File_Lookup is (File_Found, No_Such_File, Not_A_File, No_Directory);

   procedure Find_File
     (Name     : String;
      Writable : Boolean;
      File     : out Rose.Capabilities.Capability;
      Result   : out File_Lookup);
   --  For an in File argument (#344): the interface cap of the read-only
   --  File interface of the ordinary file called Name in the current
   --  directory, which is home until petal has one (#342). If Writable,
   --  for an in Writable_File argument of a live command, it is the
   --  file's Writable_File interface, so the command's changes take
   --  effect as it makes them (ADR 0012, section 4). Result says
   --  whether there is such a file. The cap is petal's until
   --  Release_Files deletes it.

   procedure Release_Files;
   --  Delete the caps that Find_File has returned since the last call,
   --  once the command they were bound for has run.

   type Version_Lookup is
     (Version_Made, Not_A_Version_File, No_Version, No_Version_Directory);

   procedure New_Version
     (Name   : String;
      File   : out Rose.Capabilities.Capability;
      Result : out Version_Lookup);
   --  For an in Writable_File argument of a transactional command (#141,
   --  ADR 0012, section 3): a version of the file called Name in the
   --  current directory, or of a new file if there is no entry Name.
   --  The version is emptied, so that the command replaces the file.
   --  File is the interface cap of the version's Writable_File. Result
   --  says whether the version was made: not if Name is not a file, or
   --  the directory refuses, or there is no directory. The version is
   --  petal's until Commit_Versions or Discard_Versions.

   procedure Commit_Versions (Failed : out Natural);
   --  Commit the versions that New_Version has made since the last call,
   --  in the order it made them, once the command they were bound for
   --  has completed. Failed is 0 if they were all committed. If not,
   --  version Failed could not be committed, and it and the ones after
   --  it are still pending, for Discard_Versions.

   function Version_Name (Index : Positive) return String;
   --  The name of the file of version Index.

   procedure Discard_Versions;
   --  Discard the versions that are still pending, as when the command
   --  they were bound for has failed, and forget them all.

   Home_Name : constant String := "home";
   --  The user's home directory, the first entry in the table. An in
   --  Directory argument that is not given is bound to it.

   procedure Get_User
     (User  : out Rose.Interfaces.User.Client.User_Client;
      Found : out Boolean);
   --  The user petal was started for, if there is one.

end Petal.Session;
