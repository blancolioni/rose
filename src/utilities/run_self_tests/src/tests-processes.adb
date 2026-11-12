with Rose.System_Calls.Own_Bank;
with Ada.Text_IO;

with Ada.Streams;
with System.Storage_Elements;

with Rose.Capabilities;

with Rose.Interfaces.Bank;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File.Client;

with Tests.Files;
with Tests.Registry;

package body Tests.Processes is

   use Ada.Streams;
   use System.Storage_Elements;

   package Files renames Rose.Interfaces.Writable_File.Client;
   package Directories renames
     Rose.Interfaces.Writable_Directory.Client;

   Hello : constant Stream_Element_Array (1 .. 5) :=
     (Character'Pos ('h'), Character'Pos ('e'), Character'Pos ('l'),
      Character'Pos ('l'), Character'Pos ('o'));

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   -------------------
   -- Bank_End_Test --
   -------------------

   function Bank_End_Test return Boolean is
      use type Rose.Capabilities.Capability;
      Own     : constant Rose.Capabilities.Capability :=
                  Rose.System_Calls.Own_Bank.Cap;
   begin
      Ada.Text_IO.Put ("Bank end test");

      if Own = Rose.Capabilities.Null_Capability then
         return Fail ("no bank of its own");
      end if;

      declare
         Own_Bank : Rose.Interfaces.Bank.Client.Bank_Client :=
                      Rose.Interfaces.Bank.Client.From_Cap (Own);
         Sub_Bank : Rose.Interfaces.Bank.Bank_Object'Class :=
                      Own_Bank.Create_Sub_Bank (0, 0);
         File     : Files.Writable_File_Client;
      begin
         File.Open (Tests.Registry.Launch_File (Sub_Bank.Bank_Cap));
         if File.Get_Interface_Cap = Rose.Capabilities.Null_Capability then
            return Fail ("no file server");
         end if;
         Tests.Files.Append (File, Hello);
         if File.Size /= Hello'Length then
            return Fail ("the file doesn't work");
         end if;

         --  Destroying the bank ends the process that was launched
         --  against it, without asking it.

         Sub_Bank.Destroy;

         begin
            Ada.Text_IO.Put (Storage_Count'Image (File.Size));
            return Fail ("the file answers after its bank was destroyed");
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;
      end;

      return True;
   end Bank_End_Test;

   ----------------------
   -- End_Process_Test --
   ----------------------

   function End_Process_Test return Boolean is
      Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
        Tests.Registry.Launcher ("directory");

      function Launch_Directory
        return Rose.Interfaces.Process.Process_Object'Class
      is (Launcher.Launch
            (Rose.System_Calls.Own_Bank.Cap,
             (Tests.Registry.Launcher_Cap ("file"),
              Tests.Registry.Launcher_Cap ("directory"),
              Tests.Registry.Cap_Sets)));
      --  A directory, which is launched with Launch and so has a Process
      --  cap, as a file did before it was a command (#867).

      First     : Rose.Interfaces.Process.Process_Object'Class :=
        Launch_Directory;
      Directory : Directories.Writable_Directory_Client;
   begin
      Ada.Text_IO.Put ("End process test");

      Directory.Open (First.Published_Interface);
      if Directory.Directory_Entry_Count /= 0 then
         return Fail ("the directory doesn't work");
      end if;

      --  The launcher ends the directory's process. The caps that are
      --  held for it then fail (#320).

      First.Destroy;

      begin
         Ada.Text_IO.Put (Natural'Image (Directory.Directory_Entry_Count));
         return Fail ("the directory answers after its process ended");
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            null;
      end;

      --  A process launched now can take the ended one's place in mem's
      --  tables, which is what a Process cap names. Ending the old one
      --  again must not reach it.

      declare
         Second           : Rose.Interfaces.Process.Process_Object'Class :=
           Launch_Directory;
         Second_Directory : Directories.Writable_Directory_Client;
      begin
         Second_Directory.Open (Second.Published_Interface);

         begin
            First.Destroy;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;

         begin
            if Second_Directory.Directory_Entry_Count /= 0 then
               return Fail ("the second directory doesn't work");
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
