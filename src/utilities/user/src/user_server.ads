with Rose.Capabilities;

package User_Server is

   --  One process serves one user through User (ADR 0003): the user's
   --  name, the user's home directory, a Writable_Directory, and a bank
   --  for each session, from the user's bank (ADR 0014, #450). The
   --  command Rose.Core.User is given all three in its launch call, sets
   --  up the server, and returns its User interface; its generated main
   --  then calls Start_Server.

   Max_Name_Length : constant := 64;

   procedure Create_Server
     (User_Name : String;
      Home      : Rose.Capabilities.Capability;
      Bank      : Rose.Capabilities.Capability);
   --  User_Name is from 1 to Max_Name_Length characters long.

   function Interface_Cap return Rose.Capabilities.Capability;
   --  The User interface, as published by Create_Server.

   procedure Start_Server;
   --  Serve the User interface. It does not return.

end User_Server;
