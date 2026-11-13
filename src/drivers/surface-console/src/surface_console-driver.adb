with Rose.System_Calls;

with Surface_Console.Server;
with Text_Console.Serial;

procedure Surface_Console.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap,
      Create_Signal   => Create_Signal_Cap);
   Text_Console.Serial.Start
     (Command_Line_Cap, Serial_Data_Cap, Serial_Status_Cap);
   Surface_Console.Server.Start_Server;
end Surface_Console.Driver;
