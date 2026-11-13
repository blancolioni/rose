with Rose.Command_Line;

with User_Server;

procedure Rose.Core.User
  (Cap_Set_Service : in out
     Rose.Interfaces.Constructor.Constructor_Object'Class;
   Name            : String;
   Home            : in out
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class;
   Bank            : in out Rose.Interfaces.Bank.Bank_Object'Class;
   User            : out Rose.Capabilities.Capability;
   Control         : out Rose.Capabilities.Capability)
is
begin

   --  An unhandled exception fails the launch call (see
   --  Rose.Command_Line.Launch), which is what a bad name should do. A
   --  version that took over keeps the old version's name, and is given
   --  none (#831).

   if not User_Server.Taken_Over
     and then (Name'Length = 0
               or else Name'Length > User_Server.Max_Name_Length)
   then
      raise Rose.Command_Line.Command_Failed with "bad name";
   end if;

   User_Server.Create_Server
     (User_Name       => Name,
      Home            => Home.Writable_Directory_Cap,
      Bank            => Bank.Bank_Cap,
      Cap_Set_Service => Cap_Set_Service.Constructor_Cap,
      Control         => Control);
   User := User_Server.Interface_Cap;
end Rose.Core.User;
