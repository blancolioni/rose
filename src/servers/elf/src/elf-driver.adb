with Rose.Console_IO;

with Elf.Server;

procedure Elf.Driver is
begin
   Rose.Console_IO.Open (Console_Cap);
   Elf.Server.Create_Server;
   Elf.Server.Start_Server;
end Elf.Driver;
