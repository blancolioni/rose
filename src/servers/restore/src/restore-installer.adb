with Rose.Console_IO;
with Rose.Directories;

with Rose.Invocation;
with Rose.System_Calls;

with Rose.Devices.Checkpoints;
with Rose.Interfaces.Stream_Reader;
with Rose.Interfaces.Stream_Reader.Client;

package body Restore.Installer is

   procedure Write_Initial_System_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   procedure Install_From_Directory
     (Directory_Entry : Rose.Directories.Directory_Entry_Type);

   procedure Install_With_Caps
     (Cap_File_Entry : Rose.Directories.Directory_Entry_Type);

   procedure Install_Executable
     (Exec_Path : String;
      Cap_Path  : String);

   -------------
   -- Install --
   -------------

   procedure Install
     (To   : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      use type Rose.Directories.File_Kind;
      Install_Directory : constant String := "/rose/install";
      Exec_Path : constant String := "/rose/install/exec";
      Exec_Cap  : constant String := "/rose/install/exec.cap";
   begin

      if not Rose.Directories.Exists (Install_Directory) then
         Rose.Console_IO.Put_Line
           ("install: cannot find directory /rose/install");
         return;
      end if;

      if Rose.Directories.Kind (Install_Directory)
        /= Rose.Directories.Directory
      then
         Rose.Console_IO.Put_Line
           ("install: /rose/install is not a directory");
      end if;

      if not Rose.Directories.Exists (Exec_Path) then
         Rose.Console_IO.Put_Line
           ("install: cannot find exec");
         return;
      end if;

      if not Rose.Directories.Exists (Exec_Cap) then
         Rose.Console_IO.Put_Line
           ("install: cannot find exec.cap");
         return;
      end if;

      Install_Executable (Exec_Path, Exec_Cap);

      Rose.Directories.Search
        (Directory => Install_Directory,
         Pattern   => "*",
         Filter    => (Rose.Directories.Directory => True, others => False),
         Process   => Install_From_Directory'Access);

      if False then
         Write_Initial_System_Image (To);
      end if;

   end Install;

   ------------------------
   -- Install_Executable --
   ------------------------

   procedure Install_Executable
     (Exec_Path : String;
      Cap_Path  : String)
   is
      use Rose.Interfaces.Stream_Reader.Client;
      Caps_Reader   : Stream_Reader_Client;
      Binary_Reader : Stream_Reader_Client;
   begin
      Rose.Directories.Open (Caps_Reader, Cap_Path);
      Rose.Directories.Open (Binary_Reader, Exec_Path);

      Rose.Console_IO.Put ("installing: ");
      Rose.Console_IO.Put (Exec_Path);
      Rose.Console_IO.Put (": ");
      Rose.Console_IO.Flush;

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Install_Exec_Cap);
         Rose.System_Calls.Send_Cap
           (Params, Get_Interface_Cap (Caps_Reader));
         Rose.System_Calls.Send_Cap
           (Params, Get_Interface_Cap (Binary_Reader));
         Rose.System_Calls.Send_Word
           (Params, Natural (Rose.Directories.Size (Exec_Path)));
         Rose.System_Calls.Receive_Caps (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);
         if Params.Control.Flags (Rose.Invocation.Error) then
            Rose.Console_IO.Put_Line ("FAIL");
         else
            Rose.Console_IO.Put_Line ("done");
            if Params.Control.Flags (Rose.Invocation.Send_Caps) then
               Rose.System_Calls.Initialize_Send (Params, Params.Caps (0));
               Rose.System_Calls.Invoke_Capability (Params);
            end if;
         end if;
      end;

   end Install_Executable;

   ----------------------------
   -- Install_From_Directory --
   ----------------------------

   procedure Install_From_Directory
     (Directory_Entry : Rose.Directories.Directory_Entry_Type)
   is
      Full_Name      : String (1 .. 100);
      Full_Name_Last : Natural;
      Simple_Name      : String (1 .. 100);
      Simple_Name_Last : Natural;
   begin
      Rose.Directories.Full_Name
        (Directory_Entry, Full_Name, Full_Name_Last);
      Rose.Directories.Simple_Name
        (Directory_Entry, Simple_Name, Simple_Name_Last);

      if Full_Name_Last = 0 then
         Rose.Console_IO.Put_Line ("directory entry: no name");
         return;
      elsif  (Simple_Name_Last = 1
              and then Simple_Name (1) = '.')
        or else (Simple_Name_Last = 2
                 and then Simple_Name (1) = '.'
                 and then Simple_Name (2) = '.')
      then
         return;
      end if;

      Rose.Console_IO.Put ("install: entering directory ");
      Rose.Console_IO.Put_Line (Full_Name (1 .. Full_Name_Last));

      Rose.Directories.Search
        (Directory => Full_Name (1 .. Full_Name_Last),
         Pattern   => "*.cap",
         Filter    =>
           (Rose.Directories.Ordinary_File => True, others => False),
         Process   => Install_With_Caps'Access);
   end Install_From_Directory;

   -----------------------
   -- Install_With_Caps --
   -----------------------

   procedure Install_With_Caps
     (Cap_File_Entry : Rose.Directories.Directory_Entry_Type)
   is
      Caps_File_Name        : String (1 .. 100);
      Caps_File_Name_Last   : Natural;
      Binary_File_Name      : String (1 .. 100);
      Binary_File_Name_Last : Natural;
   begin
      Rose.Directories.Full_Name
        (Cap_File_Entry, Caps_File_Name, Caps_File_Name_Last);
      Binary_File_Name_Last := Caps_File_Name_Last - 4;  --  "*.cap"

      Binary_File_Name (1 .. Binary_File_Name_Last) :=
        Caps_File_Name (1 .. Binary_File_Name_Last);

      if not Rose.Directories.Exists
        (Binary_File_Name (1 .. Binary_File_Name_Last))
      then
         Rose.Console_IO.Put ("install: ");
         Rose.Console_IO.Put
           (Binary_File_Name (1 .. Binary_File_Name_Last));
         Rose.Console_IO.Put (": no such file");
         Rose.Console_IO.New_Line;
         return;
      end if;

      Install_Executable
        (Exec_Path => Binary_File_Name (1 .. Binary_File_Name_Last),
         Cap_Path  => Caps_File_Name (1 .. Caps_File_Name_Last));

   end Install_With_Caps;

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
