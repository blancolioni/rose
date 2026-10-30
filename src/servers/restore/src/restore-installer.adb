with System.Storage_Elements;

with Rose.Limits;
with Rose.Words;

with Rose.Console_IO;
with Rose.Directories;

with Rose.Invocation;
with Rose.System_Calls;

with Rose.Interfaces.File_Reader.Client.Conversions;
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

   function Get_Ancestor_Interface
     (Interface_Cap : Rose.Capabilities.Capability;
      Ancestor      : Positive)
      return Rose.Capabilities.Capability;
   --  The interface cap of an ancestor of the interface of Interface_Cap,
   --  numbered as the generated server numbers them.

   Stream_Reader_Ancestor : constant := 1;
   Stream_Writer_Ancestor : constant := 2;
   --  The ancestors of Terminal (interfaces/terminal.idl).

   Clock_Ancestor         : constant := 1;
   --  The ancestor of Clock_Admin (interfaces/clock_admin.idl).

   Keyboard_Interface_Cap : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability;
   Input_Stream_Cap       : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability;
   Terminal_Cap           : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability;

   Invoke_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Rose.Limits.Page_Size)
     with Alignment => 4096;

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

   procedure Send_Console (Console_Cap : Rose.Capabilities.Capability);
   --  Give init the persistent console's Stream_Writer interface, so
   --  that it replaces the boot console before anything else is
   --  installed.

   procedure Send_Clock (Admin_Cap : Rose.Capabilities.Capability);
   --  Give init the clock's Clock_Admin interface, and its Clock
   --  ancestor. Nothing is sent if there is no Clock_Admin, because a
   --  message with no caps is the restore server's last.

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

   ----------------------------
   -- Get_Ancestor_Interface --
   ----------------------------

   function Get_Ancestor_Interface
     (Interface_Cap : Rose.Capabilities.Capability;
      Ancestor      : Positive)
      return Rose.Capabilities.Capability
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Ancestor));
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         return Rose.Capabilities.Null_Capability;
      else
         return Params.Caps (0);
      end if;
   end Get_Ancestor_Interface;

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
     (Source : Step_Iterator)
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
         Install_Flags : Install_Flag_Array := Flags;
      begin
         Install_Flags (Is_Driver) := Category = "drivers";
         Install_Executable
           (Root_Path => Install_Directory,
            Directory => Category,
            Name      => Name,
            Action    => Action,
            Flags     => Install_Flags);
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

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Init_Cap);
         Rose.System_Calls.Send_Cap (Params, Keyboard_Interface_Cap);
         Rose.System_Calls.Send_Cap (Params, Input_Stream_Cap);
         Rose.System_Calls.Send_Cap (Params, Terminal_Cap);
         Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(0));
         Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(99));

         Rose.System_Calls.Invoke_Capability (Params);
      end;

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
      use Rose.Interfaces.File_Reader.Client;
      Caps_Reader    : File_Reader_Client;
      Binary_Reader  : File_Reader_Client;
      --  Released once exec has read them, which frees their slots in
      --  isofs (ADR 0009). Restore has no cap to delete caps, so they
      --  are not closed.
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
      Append (Cap_Path, Cap_Path_Last, ".caps");

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

      Rose.Console_IO.Put ("restore: installing: ");
      Rose.Console_IO.Put (Name);
      Rose.Console_IO.Put (": ");
      Rose.Console_IO.Flush;

      declare
         use System.Storage_Elements;
         Params : aliased Rose.Invocation.Invocation_Record;
         Last   : Storage_Count := 0;
      begin

         Invoke_Buffer := (others => 0);
         for Ch of Name loop
            Last := Last + 1;
            Invoke_Buffer (Last) := Character'Pos (Ch);
         end loop;

         Rose.System_Calls.Initialize_Send (Params, Init_Cap);
         Rose.System_Calls.Send_Cap
           (Params,
            Rose.Interfaces.Stream_Reader.Client.Get_Interface_Cap
              (Conversions.To_Stream_Reader (Caps_Reader)));
         Rose.System_Calls.Send_Cap
           (Params,
            Rose.Interfaces.Stream_Reader.Client.Get_Interface_Cap
              (Conversions.To_Stream_Reader (Binary_Reader)));
         Rose.System_Calls.Send_Word
           (Params,
            Natural (Rose.Directories.Size (Exec_Path (1 .. Exec_Path_Last))));
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word'(Install_Action'Pos (Action)));
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word (Flag_Word));

         Rose.System_Calls.Send_Buffer
           (Params, Last, Invoke_Buffer'Address, False);

         if Flags (Requires_Keyboard_Interface) then
            Rose.System_Calls.Send_Cap (Params, Keyboard_Interface_Cap);
         end if;

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
                        Rose.System_Calls.Send_Text (Reply, Name);
                        Rose.System_Calls.Invoke_Capability (Reply);

                        if Flags (Provides_Keyboard_Interface) then
                           Keyboard_Interface_Cap :=
                             Get_Public_Interface_From_Process
                               (Reply.Caps (0));
                        end if;

                        if Flags (Provides_Clock) then
                           Send_Clock
                             (Get_Public_Interface_From_Process
                                (Reply.Caps (0)));
                        end if;

                        if Flags (Provides_Console) then

                           --  The console is a Terminal (#454). Init gets
                           --  its Stream_Writer as the console, its
                           --  Stream_Reader as standard input, and the
                           --  Terminal itself.

                           Terminal_Cap :=
                             Get_Public_Interface_From_Process
                               (Reply.Caps (0));
                           Send_Console
                             (Get_Ancestor_Interface
                                (Terminal_Cap, Stream_Writer_Ancestor));
                           if Flags (Provides_Input_Stream) then
                              Input_Stream_Cap :=
                                Get_Ancestor_Interface
                                  (Terminal_Cap, Stream_Reader_Ancestor);
                           end if;

                        elsif Flags (Provides_Input_Stream) then
                           Input_Stream_Cap :=
                             Get_Public_Interface_From_Process
                               (Reply.Caps (0));
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

      Release (Caps_Reader);
      Release (Binary_Reader);
   end Install_Executable;

   ----------------
   -- Send_Clock --
   ----------------

   procedure Send_Clock (Admin_Cap : Rose.Capabilities.Capability) is
      use type Rose.Capabilities.Capability;
      Clock_Cap : constant Rose.Capabilities.Capability :=
                    (if Admin_Cap = Rose.Capabilities.Null_Capability
                     then Rose.Capabilities.Null_Capability
                     else Get_Ancestor_Interface (Admin_Cap, Clock_Ancestor));
      Params    : aliased Rose.Invocation.Invocation_Record;
   begin
      if Clock_Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put_Line ("restore: no clock interface");
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Init_Cap);
      Rose.System_Calls.Send_Cap (Params, Admin_Cap);
      Rose.System_Calls.Send_Cap (Params, Clock_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(0));
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(97));
      Rose.System_Calls.Invoke_Capability (Params);
   end Send_Clock;

   ------------------
   -- Send_Console --
   ------------------

   procedure Send_Console (Console_Cap : Rose.Capabilities.Capability) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Init_Cap);
      Rose.System_Calls.Send_Cap (Params, Console_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(0));
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(98));
      Rose.System_Calls.Invoke_Capability (Params);
   end Send_Console;

end Restore.Installer;
