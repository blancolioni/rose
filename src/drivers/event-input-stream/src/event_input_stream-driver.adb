with Rose.Console_IO;
with Rose.System_Calls;

with Event_Input_Stream.Server;

procedure Event_Input_Stream.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);

   --  The line discipline echoes what is typed to the console (#397).

   Rose.Console_IO.Open (Console_Cap);
   Event_Input_Stream.Server.Start_Server;
end Event_Input_Stream.Driver;
