with Rose.Capabilities;

package Tests.Users is

   function Create_User
     (Name : String;
      Home : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;
   --  Launch a user called Name that holds the home directory Home, and
   --  return the user's User interface, or a null cap if that fails. Name
   --  is at most 255 characters. Users can share a home directory.

   function User_Test return Boolean;
   --  Launch a home directory and a user that holds it, then check the
   --  user's name, and that its home directory is that directory.

end Tests.Users;
