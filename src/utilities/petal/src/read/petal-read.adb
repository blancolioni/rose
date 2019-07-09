with Ada.Text_IO;

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
   begin
      Done := False;

      while not Done loop
         Ada.Text_IO.Put ("petal> ");
         Ada.Text_IO.Flush;
         declare
            Line   : constant String := Ada.Text_IO.Get_Line;
            Result : Petal.Parser.Parse_Result;
         begin

            Petal.Parser.Parse (Line, Result);

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
