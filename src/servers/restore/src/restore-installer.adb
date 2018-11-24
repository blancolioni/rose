with Rose.Console_IO;

package body Restore.Installer is

   -------------
   -- Install --
   -------------

   procedure Install
     (From : Rose.Interfaces.Directory.Client.Directory_Client)
   is
      use Rose.Interfaces.Directory.Client;
   begin
      for I in 1 .. Directory_Entry_Count (From) loop
         declare
            Name : String (1 .. 32) := (others => ' ');
            Last : Natural;
         begin
            Directory_Entry_Name (From, I, Name, Last);
            Rose.Console_IO.Put (Name);
            Rose.Console_IO.Put (" ");
            case Directory_Entry_Kind (From, I) is
               when Rose.Interfaces.Directory.Directory =>
                  Rose.Console_IO.Put ("d");
               when Rose.Interfaces.Directory.Ordinary_File =>
                  Rose.Console_IO.Put ("f");
               when Rose.Interfaces.Directory.Special_File =>
                  Rose.Console_IO.Put ("s");
            end case;
            Rose.Console_IO.New_Line;
         end;
      end loop;
   end Install;

end Restore.Installer;
