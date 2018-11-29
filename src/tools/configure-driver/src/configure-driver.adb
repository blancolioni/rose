with Ada.Command_Line;

with Tropos.Reader;

with Configure.Writer;

procedure Configure.Driver is
   Config : constant Tropos.Configuration :=
              Tropos.Reader.Read_Config (Ada.Command_Line.Argument (1));
begin
   Configure.Writer.Write_Configuration (Config);
end Configure.Driver;
