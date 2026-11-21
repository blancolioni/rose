with Home_Holder;

procedure Rose.Core.Hold_Home
  (Find_Command    : in out Rose.Interfaces.Commands.Lookup
    .Lookup_Object'Class;
   Cap_Set_Service : in out
     Rose.Interfaces.Caps.Bank_Constructor
       .Bank_Constructor_Object'Class;
   User_Control    : out Rose.Capabilities.Capability;
   Home_Control    : out Rose.Capabilities.Capability)
is
begin
   --  The generated main has given Create_Endpoint to Use_Capabilities
   --  and Standard_Output to Ada.Text_IO. The launch call completes, and
   --  then it runs Home_Holder.Start_Server, which never returns.

   Home_Holder.Create_Server
     (Commands        => Find_Command.Lookup_Cap,
      Cap_Set_Service => Cap_Set_Service.Bank_Constructor_Cap,
      User_Control    => User_Control,
      Home_Control    => Home_Control);
end Rose.Core.Hold_Home;
