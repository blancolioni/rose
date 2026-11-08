with Rose.Command_Line;

with User_Server;

procedure Rose.Core.User
  (Name : String;
   Home : in out
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class;
   User : out Rose.Capabilities.Capability)
is
begin

   --  An unhandled exception fails the launch call (see
   --  Rose.Command_Line.Launch), which is what a bad name should do.

   if Name'Length = 0 or else Name'Length > User_Server.Max_Name_Length
   then
      raise Rose.Command_Line.Command_Failed with "bad name";
   end if;

   User_Server.Create_Server
     (User_Name => Name,
      Home      => Home.Writable_Directory_Cap);
   User := User_Server.Interface_Cap;
end Rose.Core.User;
