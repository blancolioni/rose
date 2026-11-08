with Rose.Capabilities;

package Auth_Server is

   --  The users, and a salted hash of each one's password (#455). The
   --  list is in the process's memory, so it persists with the process.

   procedure Create_Server (Commands : Rose.Capabilities.Capability);
   --  Serve Auth and Auth_Admin. Commands, the command registry's Commands
   --  interface, finds the directory and user programs that New_User
   --  launches.

   function Auth_Cap return Rose.Capabilities.Capability;
   function Admin_Cap return Rose.Capabilities.Capability;

   procedure Start_Server;

end Auth_Server;
