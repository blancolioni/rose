with Rose.Capabilities;

with Rose.Interfaces.User.Client;
with Rose.Interfaces.Writable_Directory.Client;

package Petal.Session is

   --  The user and the directories that petal is given (ADR 0003): the
   --  user's User interface, and a table of named directories whose
   --  first entry is the user's home directory, home (ADR 0002,
   --  section 5).

   procedure Start;
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
     (Name  : String;
      Grant : out Rose.Capabilities.Capability;
      Found : out Boolean);
   --  A read-only grant of the directory called Name, for an in Directory
   --  argument: the interface cap of its Directory interface, which has
   --  only the lookup operations (ADR 0003, section 1). It is made the
   --  first time it is asked for, and then kept.

   type File_Lookup is (File_Found, No_Such_File, Not_A_File, No_Directory);

   procedure Find_File
     (Name   : String;
      File   : out Rose.Capabilities.Capability;
      Result : out File_Lookup);
   --  For an in File argument (#344): the interface cap of the read-only
   --  File interface of the ordinary file called Name in the current
   --  directory, which is home until petal has one (#342). Result says
   --  whether there is such a file. The cap is petal's until
   --  Release_Files deletes it.

   procedure Release_Files;
   --  Delete the caps that Find_File has returned since the last call,
   --  once the command they were bound for has run.

   Home_Name : constant String := "home";
   --  The user's home directory, the first entry in the table. An in
   --  Directory argument that is not given is bound to it.

   procedure Get_User
     (User  : out Rose.Interfaces.User.Client.User_Client;
      Found : out Boolean);
   --  The user petal was started for, if there is one.

end Petal.Session;
