with Rose.System_Calls;

with Text_Console.Serial;
with VGA_Console.Server;

procedure VGA_Console.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Text_Console.Serial.Start
     (Command_Line_Cap, Serial_Data_Cap, Serial_Status_Cap);
   VGA_Console.Server.Start_Server;
end VGA_Console.Driver;
