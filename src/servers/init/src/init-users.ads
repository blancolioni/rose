with Rose.Capabilities;
with Rose.Words;

package Init.Users is

   --  Users are created and checked by the auth server (#455), and what
   --  they may have is decided by the rights broker (ADR 0021).

   Mode_None    : constant Rose.Words.Word := 0;
   Mode_Elevate : constant Rose.Words.Word := 1;
   Mode_Granted : constant Rose.Words.Word := 2;
   --  The modes of Rights_Admin.Allow.

   procedure Start_Auth
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Auth            : out Rose.Capabilities.Capability;
      Admin           : out Rose.Capabilities.Capability);
   --  Launch auth as a command, and return its Auth and Auth_Admin
   --  interfaces, or null caps if the launch fails.

   Max_Name_Length     : constant := 64;
   Max_Password_Length : constant := 128;

   procedure Ask_First_User
     (Console       : Rose.Capabilities.Capability;
      Output        : Rose.Capabilities.Capability;
      Name          : out String;
      Name_Last     : out Natural;
      Password      : out String;
      Password_Last : out Natural);
   --  Name is Max_Name_Length characters, and Password
   --  Max_Password_Length, both from 1.
   --
   --  Ask the person installing for the first user's name and password
   --  (#456), on Console, the console's Terminal interface, writing the
   --  questions to Output, its stream. The name is asked for until it is
   --  from 1 to Max_Name_Length letters, digits, '_', '-' or '.'. The
   --  password is read with echo off, and then again, until the two are
   --  the same and not empty.

   procedure Wait_For_Enter
     (Console : Rose.Capabilities.Capability;
      Output  : Rose.Capabilities.Capability;
      Message : String);
   --  Write Message to Output, and wait for a line from Console, the
   --  console's Terminal interface.

   procedure Create_First_User
     (Name     : String;
      Password : String;
      Admin    : Rose.Capabilities.Capability;
      User     : out Rose.Capabilities.Capability;
      Home     : out Rose.Capabilities.Capability);
   --  Create the first user through Admin, an Auth_Admin interface, and
   --  give the user a .petalrc. User and Home are null if it fails.

   function Check_Login
     (Auth     : Rose.Capabilities.Capability;
      Name     : String;
      Password : String)
      return Boolean;
   --  True if Auth.Login gives a user for Name and Password.

   function Start_Login
     (Find_Cap        : Rose.Capabilities.Capability;
      Console         : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Timer           : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability;
      Rights          : Rose.Capabilities.Capability;
      Auth            : Rose.Capabilities.Capability)
      return Boolean;
   --  Launch login as a command on the console (#457). It completes its
   --  launch call and then keeps running. False if it can't be launched.

   procedure Start_Rights
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Console         : Rose.Capabilities.Capability;
      Clock_Admin     : Rose.Capabilities.Capability;
      Rights          : out Rose.Capabilities.Capability;
      Admin           : out Rose.Capabilities.Capability);
   --  Launch rights, the rights broker (ADR 0021), as a command, and
   --  return its Rights and Rights_Admin interfaces, or null caps if the
   --  launch fails. Console is its standard output, and Clock_Admin the
   --  clock's Clock_Admin interface, which it holds as its
   --  Clock_Admin_Service role. They are the caps of its roles, in the
   --  order of src/utilities/rights/rose-core-rights.idl.

   function Allow
     (Admin    : Rose.Capabilities.Capability;
      User     : String;
      Category : String;
      Mode     : Rose.Words.Word)
      return Boolean;
   --  Set how User may get Category, through Admin, a Rights_Admin
   --  interface. "*" is every category. True if the broker set it.

   function Check_Rights
     (Rights : Rose.Capabilities.Capability;
      User   : Rose.Capabilities.Capability;
      Role   : String)
      return Boolean;
   --  True if the broker, through Rights, a Rights interface, refuses the
   --  role Role to User, a User interface, in an ordinary request, and
   --  grants it under sudo, as a policy of elevate does. The grant is
   --  finished again.

end Init.Users;
