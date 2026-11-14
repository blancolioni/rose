with Ada.Streams;

with Rose.Capabilities;
with Rose.Replacement;

package Auth_Server is

   --  The users, and a salted hash of each one's password (#455). The
   --  list is in the process's memory, so it persists with the process.
   --  A new version of the program can take over from a running one
   --  (ADR 0032, #831), with the users, their caps and their logins.

   procedure Write_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps);
   --  The users, for a new version or a save.

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps);
   --  The state of the version that this one takes over from, or of a
   --  saved one, which the main reads before it calls the command. It
   --  raises Bad_State if a user's caps are missing, as they are from a
   --  saved state.

   procedure Create_Server
     (Commands        : Rose.Capabilities.Capability;
      Cap_Set_Service : Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability);
   --  Serve Auth and Auth_Admin, each with the identifier that the state
   --  had, and Replaceable, whose cap is Control. Commands, the command
   --  registry's Commands interface, finds the directory and user
   --  programs that New_User launches. Cap_Set_Service, the cap set
   --  server's Constructor, is given to each home directory and user
   --  (#867, #831).

   function Auth_Cap return Rose.Capabilities.Capability;
   function Admin_Cap return Rose.Capabilities.Capability;

   procedure Start_Server;

end Auth_Server;
