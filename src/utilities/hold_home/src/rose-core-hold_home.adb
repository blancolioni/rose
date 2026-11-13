with Home_Holder;

procedure Rose.Core.Hold_Home
  (Find_Command    : in out Rose.Interfaces.Commands.Commands_Object'Class;
   Cap_Set_Service : in out
     Rose.Interfaces.Constructor.Constructor_Object'Class;
   User_Control    : out Rose.Capabilities.Capability)
is
begin
   --  The generated main has given Create_Endpoint to Use_Capabilities
   --  and Standard_Output to Ada.Text_IO. The launch call completes, and
   --  then it runs Home_Holder.Start_Server, which never returns.

   Home_Holder.Create_Server
     (Commands        => Find_Command.Commands_Cap,
      Cap_Set_Service => Cap_Set_Service.Constructor_Cap,
      User_Control    => User_Control);
end Rose.Core.Hold_Home;
