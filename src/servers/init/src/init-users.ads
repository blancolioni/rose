with Rose.Capabilities;

package Init.Users is

   procedure Create_First_User
     (Name            : String;
      Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      User            : out Rose.Capabilities.Capability;
      Home            : out Rose.Capabilities.Capability);
   --  Create a home directory holding a .petalrc that welcomes the user,
   --  and launch a user process called Name that holds it (ADR 0003).
   --  The programs are found through the command registry's Find_Cap,
   --  and the user is given Create_Endpoint to serve its interface.
   --  User is the user's User interface, and Home its home directory, a
   --  Writable_Directory. Both are null if anything fails.

end Init.Users;
