with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;

with Rose.Interfaces.Launch;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;
with Rose.Interfaces.Writable_File.Client;

with Tests.Registry;

package body Tests.Processes is

   use System.Storage_Elements;

   package Files renames Rose.Interfaces.Writable_File.Client;

   Hello : constant Storage_Array (1 .. 5) :=
     (Character'Pos ('h'), Character'Pos ('e'), Character'Pos ('l'),
      Character'Pos ('l'), Character'Pos ('o'));

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   ----------------------
   -- End_Process_Test --
   ----------------------

   function End_Process_Test return Boolean is
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
        Tests.Registry.Launcher ("file");
      First    : Rose.Interfaces.Process.Process_Object'Class :=
        Launcher.Launch (No_Caps);
      File     : Files.Writable_File_Client;
   begin
      Ada.Text_IO.Put ("End process test");

      File.Open (First.Published_Interface);
      File.Write (Hello);
      if File.Size /= Hello'Length then
         return Fail ("the file doesn't work");
      end if;

      --  The launcher ends the file's process. The caps that are held for
      --  it then fail (#320).

      First.Destroy;

      begin
         Ada.Text_IO.Put (Storage_Count'Image (File.Size));
         return Fail ("the file answers after its process ended");
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            null;
      end;

      --  A process launched now can take the ended one's place in mem's
      --  tables, which is what a Process cap names. Ending the old one
      --  again must not reach it.

      declare
         Second      : Rose.Interfaces.Process.Process_Object'Class :=
           Launcher.Launch (No_Caps);
         Second_File : Files.Writable_File_Client;
      begin
         Second_File.Open (Second.Published_Interface);

         begin
            First.Destroy;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;

         begin
            Second_File.Write (Hello);
            if Second_File.Size /= Hello'Length then
               return Fail ("wrong size in the second file");
            end if;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               return Fail ("the old process cap ended a new process");
         end;

         Second.Destroy;
      end;

      return True;
   end End_Process_Test;

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Message);
      return False;
   end Fail;

end Tests.Processes;
