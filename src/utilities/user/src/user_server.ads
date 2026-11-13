with Ada.Streams;

with Rose.Capabilities;
with Rose.Replacement;

package User_Server is

   --  One process serves one user through User (ADR 0003): the user's
   --  name, the user's home directory, a Writable_Directory, and a bank
   --  for each session, from the user's bank (ADR 0014, #450). The
   --  command Rose.Core.User is given all three in its launch call, sets
   --  up the server, and returns its User interface; its generated main
   --  then calls Start_Server.
   --
   --  A new version of the program can take over from a running one
   --  (ADR 0032, #831), and a saved state can be restored (section 10).

   Max_Name_Length : constant := 64;

   procedure Write_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps);
   --  The user's state, for a new version or a save.

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps);
   --  The state of the version that this one takes over from, or of a
   --  saved one, which the main reads before it calls the command.

   function Taken_Over return Boolean;
   --  True if the main read a state, whose name the server keeps, and
   --  not the one that the command was given.

   procedure Create_Server
     (User_Name       : String;
      Home            : Rose.Capabilities.Capability;
      Bank            : Rose.Capabilities.Capability;
      Cap_Set_Service : Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability);
   --  Attach the User interface, with the identifier that the state had,
   --  and the Replaceable interface, whose cap is Control. User_Name is
   --  from 1 to Max_Name_Length characters long, and is used only by a
   --  server that neither took over nor restored.

   function Interface_Cap return Rose.Capabilities.Capability;
   --  The User interface, as attached by Create_Server.

   procedure Start_Server;
   --  Serve the User interface. It does not return.

end User_Server;
