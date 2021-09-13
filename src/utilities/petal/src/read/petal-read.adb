with Ada_Text_IO;

with Petal.Commands;
with Petal.Parser;

package body Petal.Read is

   Done : Boolean;

   ----------------------
   -- Exit_Reader_Loop --
   ----------------------

   procedure Exit_Reader_Loop is
   begin
      Done := True;
   end Exit_Reader_Loop;

   -----------------
   -- Reader_Loop --
   -----------------

   procedure Reader_Loop is
      use Ada_Text_IO;
      Line : String (1 .. 2000);
      Last : Natural;
   begin
      Done := False;

      while not Done loop
         Put ("petal> ");
         Flush;

         if End_Of_File then
            Put_Line ("logout");
            exit;
         end if;

         Get_Line (Line, Last);

         declare
            Result : Petal.Parser.Parse_Result;
         begin
            Petal.Parser.Parse (Line (1 .. Last), Result);

            while Petal.Parser.Has_Element (Result) loop
               declare
                  Element : constant Petal.Parser.Parse_Element :=
                              Petal.Parser.Element (Result);
               begin
                  Petal.Commands.Execute
                    (Command   => Petal.Parser.Command (Element),
                     Context   => Petal.Parser.Context (Element),
                     Arguments => Petal.Parser.Arguments (Element));
                  Petal.Parser.Next (Result);
               end;
            end loop;
         end;
      end loop;
   end Reader_Loop;

end Petal.Read;
