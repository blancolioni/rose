with Ada_Text_IO;

with Petal.Command_Launch;
with Petal.Commands;
with Petal.Exec;
with Petal.Parser;

package body Petal.Read is

   Done : Boolean;

   type Line_Access is access String;

   Line : constant Line_Access := new String (1 .. 2000);
   --  The line being read. It is on the heap, so that petal working after
   --  a restore shows that its heap came back (#358).

   -------------
   -- Execute --
   -------------

   procedure Execute (Line : String) is

      function Built_In return Boolean;
      --  True if the line starts with the name of a built-in command.

      --------------
      -- Built_In --
      --------------

      function Built_In return Boolean is
         First : Positive := Line'First;
         Stop  : Natural;
      begin
         while First <= Line'Last and then Line (First) = ' ' loop
            First := First + 1;
         end loop;
         Stop := First - 1;
         while Stop < Line'Last and then Line (Stop + 1) /= ' ' loop
            Stop := Stop + 1;
         end loop;
         return Petal.Exec.Is_Built_In (Line (First .. Stop));
      end Built_In;

   begin

      --  Built-in commands are parsed and run here; anything else is a
      --  command from the registry.

      if not Built_In then
         Petal.Command_Launch.Run (Line);
      else
         declare
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
      end if;
   end Execute;

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
      Last : Natural;
   begin
      Done := False;

      while not Done loop
         Put ("petal> ");
         Flush;

         --  An end of file ends the session: petal logs out, and its
         --  command completes, so that login asks for the next user
         --  (#458).

         if End_Of_File then
            New_Line;
            Put_Line ("logout");
            exit;
         end if;

         Get_Line (Line.all, Last);
         Execute (Line (1 .. Last));
      end loop;
   end Reader_Loop;

end Petal.Read;
