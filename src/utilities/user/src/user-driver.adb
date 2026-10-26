with Rose.Command_Line.Launch;
with Rose.System_Calls;

with User.Server;

procedure User.Driver is
   use Rose.Command_Line.Launch;
   Call : Launch_Call;
begin

   --  The user starts like a command, but stays to serve its interface
   --  once it has replied.

   Receive (Call);
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Cap_Argument (Call, Create_Endpoint_Argument));

   declare
      Name : constant String := String_Argument (Call, Name_Item);
   begin
      if Name'Length = 0 or else Name'Length > User.Server.Max_Name_Length
      then
         Fail (Call, "user: bad name");
         return;
      end if;

      User.Server.Create_Server
        (User_Name => Name,
         Home      => Cap_Argument (Call, Home_Argument));
   end;

   Cap_Result (Call, User_Result, User.Server.Interface_Cap);
   Complete (Call);
   User.Server.Start_Server;
end User.Driver;
