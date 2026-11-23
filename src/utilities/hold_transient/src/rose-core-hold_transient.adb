with Transient_Holder;

procedure Rose.Core.Hold_Transient is
begin
   --  The generated main has given Create_Endpoint to Use_Capabilities
   --  and Standard_Output to Ada.Text_IO. The launch call completes once
   --  the marker is written, and then it runs Transient_Holder
   --  .Start_Server, which never returns.

   Transient_Holder.Create_Server;
end Rose.Core.Hold_Transient;
