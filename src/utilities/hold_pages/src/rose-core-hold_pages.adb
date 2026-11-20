with Page_Holder;

procedure Rose.Core.Hold_Pages is
begin
   --  The generated main has given Create_Endpoint to Use_Capabilities
   --  and Standard_Output to Ada.Text_IO. The launch call completes once
   --  the pages are filled, and then it runs Page_Holder.Start_Server,
   --  which never returns.

   Page_Holder.Create_Server;
end Rose.Core.Hold_Pages;
