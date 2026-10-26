package User is

   pragma Pure (User);

   --  The launch call of a user (Rose.Command_Line.Launch): the caller
   --  launches the program with Launch_Command, and calls its endpoint
   --  with a create endpoint cap, the user's home directory, and the
   --  user's name. The reply holds the user's User interface.

   Create_Endpoint_Argument : constant := 0;
   Home_Argument            : constant := 1;
   --  Caps of the launch call. The home directory is a
   --  Writable_Directory.

   Name_Item : constant := 1;
   --  The buffer item of the launch call that holds the name.

   User_Result : constant := 0;
   --  The cap of the reply that holds the User interface.

end User;
