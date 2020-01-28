with Ada.Command_Line;
with Ada.Text_IO;

with IDL.Options;

with IDL.Endpoints;
with IDL.Generate;
with IDL.Generate_Kernel;
with IDL.Generate_Init;
with IDL.Procs;
with IDL.Parser;
with IDL.Syntax;
with IDL.Types;

procedure IDL.Main is
begin
   if Ada.Command_Line.Argument_Count not in 1 .. 2 then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Usage: idl <idl-file-name> [interface-table-path]");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   if Ada.Command_Line.Argument_Count = 2 then
      IDL.Endpoints.Set_Table_Path (Ada.Command_Line.Argument (2));
   end if;

   IDL.Types.Create_Standard_Types;
   if IDL.Options.Generate_Script then
      declare
         Proc : constant IDL.Procs.IDL_Procedure :=
                  IDL.Parser.Parse_Petal_File
                    (Ada.Command_Line.Argument (1));
      begin
         IDL.Generate_Init.Generate_Init_Script
           (Proc, "init-script.ads");
      end;
   else
      declare
         Defn : IDL.Syntax.IDL_Interface;
      begin
         Defn :=
           IDL.Parser.Parse_Interface_File
             (Ada.Command_Line.Argument (1));
         if IDL.Options.Generate_Kernel_Interface then
            IDL.Generate_Kernel.Generate_Kernel_Interface (Defn);
         else
            IDL.Generate.Generate_Interface (Defn);
         end if;
      end;
   end if;

exception
   when IDL.Parser.Parse_Error =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "errors detected");
end IDL.Main;
