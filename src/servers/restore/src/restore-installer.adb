with Rose.Words;

with Rose.Console_IO;
with Rose.Directories;

with Rose.Invocation;
with Rose.System_Calls;

with Rose.Devices.Checkpoints;
with Rose.Interfaces.Stream_Reader;
with Rose.Interfaces.Stream_Reader.Client;

package body Restore.Installer is

   type Array_Of_Capabilities is
     array (Positive range <>) of Rose.Capabilities.Capability;

   procedure Get_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities);

   function Get_Public_Interface_From_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   procedure Write_Initial_System_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   Keyboard_Interface_Cap : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability
                                with Unreferenced;

   --  procedure Install_From_Directory
   --    (Directory_Entry : Rose.Directories.Directory_Entry_Type);
   --
   --  procedure Install_With_Caps
   --    (Cap_File_Entry : Rose.Directories.Directory_Entry_Type);

   procedure Install_Executable
     (Root_Path : String;
      Directory : String;
      Name      : String;
      Action    : Install_Action;
      Flags     : Install_Flag_Array);

   procedure Append
     (To   : in out String;
      Last : in out Natural;
      S    : String);

   ------------
   -- Append --
   ------------

   procedure Append
     (To   : in out String;
      Last : in out Natural;
      S    : String)
   is
   begin
      for Ch of S loop
         Last := Last + 1;
         To (Last) := Ch;
      end loop;
   end Append;

   -------------------
   -- Get_Interface --
   -------------------

   procedure Get_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send             => True,
                               Block            => True,
                               Recv_Caps        => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Control.Last_Recv_Cap :=
        Capability_Index (Interface_Caps'Length - 1);

      Rose.System_Calls.Invoke_Capability (Params);

      Interface_Caps := (others => 0);

      if not (Params.Control.Flags (Error)
              or else not Params.Control.Flags (Send_Caps))
      then
         declare
            Last : Natural := Interface_Caps'First - 1;
         begin
            for Index in 0 .. Params.Control.Last_Sent_Cap loop
               Last := Last + 1;
               exit when Last > Interface_Caps'Length;
               Interface_Caps (Last) := Params.Caps (Index);
            end loop;
         end;
      end if;
   end Get_Interface;

   ---------------------------------------
   -- Get_Public_Interface_From_Process --
   ---------------------------------------

   function Get_Public_Interface_From_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      use Rose.Capabilities;
      Caps          : Array_Of_Capabilities (1 .. 7);
      Public_Cap    : Rose.Capabilities.Capability;
      Interface_Cap : Rose.Capabilities.Capability :=
                        Rose.Capabilities.Null_Capability;
   begin
      --  get process interface caps
      Get_Interface (Process_Cap, Caps);

      --  public interface cap is in Caps (3)
      Public_Cap := Caps (3);

      if Public_Cap /= Null_Capability then
         Get_Interface (Public_Cap, Caps);
         Interface_Cap := Caps (1);
      end if;

      return Interface_Cap;
   end Get_Public_Interface_From_Process;

   -------------
   -- Install --
   -------------

   procedure Install
     (To     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Source : Step_Iterator)
   is
      use type Rose.Directories.File_Kind;
      Install_Directory : constant String := "/rose/install";

      procedure Process_Executable
        (Name     : String;
         Category : String;
         Action   : Install_Action;
         Flags    : Install_Flag_Array);

      ------------------------
      -- Process_Executable --
      ------------------------

      procedure Process_Executable
        (Name     : String;
         Category : String;
         Action   : Install_Action;
         Flags    : Install_Flag_Array)
      is
      begin
         Install_Executable
           (Root_Path => Install_Directory,
            Directory => Category,
            Name      => Name,
            Action    => Action,
            Flags     => Flags);
      end Process_Executable;

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

      --  Process_Executable ("exec", "", Launch);
      --  Process_Executable ("command", "drivers", Launch)

      --  Launch_Executable (Exec_Path, Exec_Cap);
      --  Launch_Executable (Command_Path, Command_Cap);

      Source (Process_Executable'Access);

      if False then
         Write_Initial_System_Image (To);
      end if;

   end Install;

   ------------------------
   -- Install_Executable --
   ------------------------

   procedure Install_Executable
     (Root_Path : String;
      Directory : String;
      Name      : String;
      Action    : Install_Action;
      Flags     : Install_Flag_Array)
   is
      use Rose.Interfaces.Stream_Reader.Client;
      Caps_Reader    : Stream_Reader_Client;
      Binary_Reader  : Stream_Reader_Client;
      Exec_Path      : String (1 .. 200);
      Exec_Path_Last : Natural := 0;
      Cap_Path       : String (1 .. 200);
      Cap_Path_Last  : Natural := 0;
      Flag_Word      : Natural := 0;
      Flag_Bit       : Positive := 1;
   begin

      for Flag of Flags loop
         if Flag then
            Flag_Word := Flag_Word + Flag_Bit;
         end if;
         Flag_Bit := Flag_Bit * 2;
      end loop;

      Append (Exec_Path, Exec_Path_Last, Root_Path);
      Append (Exec_Path, Exec_Path_Last, "/");
      if Directory /= "" then
         Append (Exec_Path, Exec_Path_Last, Directory);
         Append (Exec_Path, Exec_Path_Last, "/");
      end if;
      Append (Exec_Path, Exec_Path_Last, Name);
      Cap_Path := Exec_Path;
      Cap_Path_Last := Exec_Path_Last;
      Append (Cap_Path, Cap_Path_Last, ".cap");

      if not Rose.Directories.Exists (Exec_Path (1 .. Exec_Path_Last)) then
         Rose.Console_IO.Put ("install: cannot find ");
         Rose.Console_IO.Put (Exec_Path (1 .. Exec_Path_Last));
         Rose.Console_IO.New_Line;
         return;
      end if;

      if not Rose.Directories.Exists (Cap_Path (1 .. Cap_Path_Last)) then
         Rose.Console_IO.Put ("install: cannot find ");
         Rose.Console_IO.Put (Cap_Path (1 .. Cap_Path_Last));
         Rose.Console_IO.New_Line;
         return;
      end if;

      Rose.Directories.Open (Caps_Reader, Cap_Path (1 .. Cap_Path_Last));
      Rose.Directories.Open (Binary_Reader, Exec_Path (1 .. Exec_Path_Last));

      Rose.Console_IO.Put ("installing: ");
      Rose.Console_IO.Put (Exec_Path (1 .. Exec_Path_Last));
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
           (Params,
            Natural (Rose.Directories.Size (Exec_Path (1 .. Exec_Path_Last))));
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word'(Install_Action'Pos (Action)));
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word (Flag_Word));
         Rose.System_Calls.Receive_Caps (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);
         if Params.Control.Flags (Rose.Invocation.Error) then
            Rose.Console_IO.Put_Line ("FAIL");
         else
            Rose.Console_IO.Put_Line ("done");
            if Params.Control.Flags (Rose.Invocation.Send_Caps) then
               declare
                  Reply : aliased Rose.Invocation.Invocation_Record;
               begin
                  case Action is
                     when Launch =>
                        Rose.System_Calls.Initialize_Send
                          (Reply, Params.Caps (0));
                        Rose.System_Calls.Receive_Caps (Reply, 1);
                        Rose.System_Calls.Invoke_Capability (Reply);

                        if Flags (Provides_Keyboard_Interface) then
                           Keyboard_Interface_Cap :=
                             Get_Public_Interface_From_Process
                               (Reply.Caps (0));
                           Rose.Console_IO.Put_Line
                             ("restore: saved keyboard interface");
                        end if;

                     when Save =>
                        Rose.System_Calls.Initialize_Send
                          (Reply, Params.Caps (0));
                        Rose.System_Calls.Send_Text (Reply, Name);
                        Rose.System_Calls.Send_Cap (Reply, Params.Caps (1));
                        Rose.System_Calls.Invoke_Capability (Reply);
                  end case;
               end;
            end if;
         end if;
      end;

   end Install_Executable;

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
