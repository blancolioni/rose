with Rose.Console_IO;
with Rose.Version;

with Terminal.Server;

procedure Terminal.Driver is
begin
   Rose.Console_IO.Open (Output_Device_Cap);
   Rose.Console_IO.Put_Line (Rose.Version.Full_Name);

   Terminal.Server.Start_Terminal;
end Terminal.Driver;
