with Rose.Capabilities;

package Init.Users is

   --  Users are created and checked by the auth server (#455).

   procedure Start_Auth
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Auth            : out Rose.Capabilities.Capability;
      Admin           : out Rose.Capabilities.Capability);
   --  Launch auth as a command, and return its Auth and Auth_Admin
   --  interfaces, or null caps if the launch fails.

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

end Init.Users;
