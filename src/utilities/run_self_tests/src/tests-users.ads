with Rose.Capabilities;

package Tests.Users is

   procedure Start_User
     (Endpoint : Rose.Capabilities.Capability;
      Name     : String;
      Home     : Rose.Capabilities.Capability;
      Bank     : Rose.Capabilities.Capability;
      User     : out Rose.Capabilities.Capability;
      Control  : out Rose.Capabilities.Capability;
      Previous : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability);
   --  Make the launch call of the user command whose endpoint is Endpoint
   --  (src/utilities/user/rose-core-user.idl), and return its User and
   --  Replaceable interfaces. If Previous is a cap, the new version takes
   --  over from it, and is given no other caps, and Name is ignored, since
   --  it takes them from the old version (#901). Raises Invocation_Error
   --  if the call fails.

   function Create_User
     (Name : String;
      Home : Rose.Capabilities.Capability;
      Bank : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
      return Rose.Capabilities.Capability;
   --  Launch a user called Name that holds the home directory Home, and
   --  return the user's User interface, or a null cap if that fails. Name
   --  is at most 255 characters. Users can share a home directory. The
   --  user is launched against Bank, and its sessions are sub-banks of
   --  it; if Bank is null, it is this process's own bank.

   function User_Test return Boolean;
   --  Launch a home directory and a user that holds it, then check the
   --  user's name, and that its home directory is that directory.

   function Session_Test return Boolean;
   --  Launch a user against a bank of its own, start a session, and
   --  launch a file server against the session's bank. Then end the
   --  session by destroying its bank, and check that the server has
   --  ended and that its storage was released from the user's bank
   --  (ADR 0014, section 2; #450).

end Tests.Users;
