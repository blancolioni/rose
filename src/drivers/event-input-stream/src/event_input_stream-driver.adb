with Rose.Console_IO;

with Event_Input_Stream.Server;

procedure Event_Input_Stream.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Event_Input_Stream.Server.Start_Server;
end Event_Input_Stream.Driver;
