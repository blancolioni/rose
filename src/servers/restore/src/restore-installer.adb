with Rose.Console_IO;

with Rose.Devices.Checkpoints;

package body Restore.Installer is

   procedure Write_Initial_System_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   -------------
   -- Install --
   -------------

   procedure Install
     (From : Rose.Interfaces.Directory.Client.Directory_Client;
      To   : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      use Rose.Interfaces.Directory.Client;
   begin
      Write_Initial_System_Image (To);
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

   --------------------------------
   -- Write_Initial_System_Image --
   --------------------------------

   procedure Write_Initial_System_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
   begin
      Rose.Devices.Checkpoints.Start_System_Image (Device);
      Rose.Devices.Checkpoints.Finish_System_Image (Device);
   end Write_Initial_System_Image;

end Restore.Installer;
