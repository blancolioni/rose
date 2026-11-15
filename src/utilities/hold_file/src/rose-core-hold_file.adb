with File_Holder;

procedure Rose.Core.Hold_File
  (Find_Command    : in out Rose.Interfaces.Commands.Lookup
    .Lookup_Object'Class;
   Cap_Set_Service : in out
     Rose.Interfaces.Caps.Constructor.Constructor_Object'Class;
   Control         : out Rose.Capabilities.Capability)
is
begin
   --  The generated main has given Create_Endpoint and Delete_Cap to
   --  Use_Capabilities and Standard_Output to Ada.Text_IO. The launch call
   --  completes, and then it runs File_Holder.Start_Server, which never
   --  returns.

   File_Holder.Create_Server
     (Commands        => Find_Command.Lookup_Cap,
      Cap_Set_Service => Cap_Set_Service.Constructor_Cap,
      Control         => Control);
end Rose.Core.Hold_File;
