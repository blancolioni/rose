with User_Server;

procedure Rose.Core.User
  (Name : String;
   Home : Rose.Interfaces.Writable_Directory.Client
            .Writable_Directory_Client;
   User : out Rose.Capabilities.Capability)
is
begin

   --  An unhandled exception fails the launch call (see
   --  Rose.Command_Line.Launch), which is what a bad name should do.

   if Name'Length = 0 or else Name'Length > User_Server.Max_Name_Length
   then
      pragma Warnings (Off, "*No_Exception_Propagation*");
      pragma Warnings (Off, "*Last_Chance_Handler*");
      raise Constraint_Error with "user: bad name";
      pragma Warnings (On, "*Last_Chance_Handler*");
      pragma Warnings (On, "*No_Exception_Propagation*");
   end if;

   User_Server.Create_Server
     (User_Name => Name,
      Home      =>
        Rose.Interfaces.Writable_Directory.Client.Get_Interface_Cap
          (Home));
   User := User_Server.Interface_Cap;
end Rose.Core.User;
