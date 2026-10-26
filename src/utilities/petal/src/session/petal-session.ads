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

   procedure Get_User
     (User  : out Rose.Interfaces.User.Client.User_Client;
      Found : out Boolean);
   --  The user petal was started for, if there is one.

end Petal.Session;
