with Rose.Capabilities;

package User.Server is

   --  One process serves one user through User (ADR 0003): the user's
   --  name, and the user's home directory, a Writable_Directory. Both
   --  are given in the launch call.

   Max_Name_Length : constant := 64;

   procedure Create_Server
     (User_Name : String;
      Home      : Rose.Capabilities.Capability);
   --  User_Name is from 1 to Max_Name_Length characters long.

   function Interface_Cap return Rose.Capabilities.Capability;
   --  The User interface, as published by Create_Server.

   procedure Start_Server;

end User.Server;
