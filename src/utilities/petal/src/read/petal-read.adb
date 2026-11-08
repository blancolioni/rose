with Ada_Text_IO;

with Petal.Command_Launch;

package body Petal.Read is

   type Line_Access is access String;

   Line : constant Line_Access := new String (1 .. 2000);
   --  The line being read. It is on the heap, so that petal working after
   --  a restore shows that its heap came back (#358).

   -------------
   -- Execute --
   -------------

   procedure Execute (Line : String) is
   begin
      Petal.Command_Launch.Run (Line);
   end Execute;

   -----------------
   -- Reader_Loop --
   -----------------

   procedure Reader_Loop is
      use Ada_Text_IO;
      Last : Natural;
   begin
      loop
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
