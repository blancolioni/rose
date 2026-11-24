with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;

with Rose.Interfaces.Users.Auth.Client;
with Rose.Interfaces.Users.Auth_Admin.Client;
with Rose.Interfaces.Processes.Launch.Client;
with Rose.Interfaces.Users.Rights.Client;
with Rose.Interfaces.Streams.Releasable_Writer.Client;
with Rose.Interfaces.Users.Rights_Admin.Client;
with Rose.Interfaces.Screens.Supervisor.Client;
with Rose.Interfaces.Screens.Terminal.Client;
with Rose.Interfaces.Users.User.Client;
with Rose.Interfaces.Files.Writable_Directory.Client;
with Rose.Interfaces.Files.Writable_File.Client;
with Rose.Interfaces.Streams.Stream.Client;
with Rose.Interfaces.Files.Linking_Directory.Client;

with Init.Calls;

package body Init.Users is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   Petal_Rc : constant String := "echo Welcome to Rose" & Character'Val (10);

   Completed : constant Rose.Words.Word := 0;

   NL : constant String := (1 => Character'Val (10));

   procedure Read_Line
     (Console : Rose.Interfaces.Screens.Terminal.Client.Terminal_Client;
      Line    : out String;
      Last    : out Natural);
   --  The next line typed on Console, without its line break. With line
   --  editing on, the console's Read gives whole lines. A line longer
   --  than Line is cut short, and an end of file is an empty line.

   procedure Forget
     (Console : in out Rose.Interfaces.Screens.Terminal.Client
       .Terminal_Client);
   --  Delete the caps that opening Console got for its subprograms, but
   --  not its interface cap, which is init's console, and which login is
   --  given later: the client's Close would delete that too. The
   --  installer is short of caps (#667), so none are left behind.

   -----------
   -- Allow --
   -----------

   function Allow
     (Admin    : Rose.Capabilities.Capability;
      User     : String;
      Category : String;
      Mode     : Rose.Words.Word)
      return Boolean
   is
      use type Rose.Words.Word;
      Client : Rose.Interfaces.Users.Rights_Admin.Client.Rights_Admin_Client;
   begin
      if Admin = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.Interfaces.Users.Rights_Admin.Client.Open (Client, Admin);
      return Rose.Interfaces.Users.Rights_Admin.Client.Allow
        (Client, User, Category, Mode) = 1;
   end Allow;

   --------------------
   -- Ask_First_User --
   --------------------

   procedure Ask_First_User
     (Console       : Rose.Capabilities.Capability;
      Output        : Rose.Capabilities.Capability;
      Name          : out String;
      Name_Last     : out Natural;
      Password      : out String;
      Password_Last : out Natural)
   is
      Terminal : Rose.Interfaces.Screens.Terminal.Client.Terminal_Client;

      function Valid_Name return Boolean
      is (Name_Last >= Name'First
          and then (for all Ch of Name (Name'First .. Name_Last) =>
                      Ch in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
                          | '_' | '-' | '.'));

   begin
      Name_Last := 0;
      Password_Last := 0;

      --  With no console there is no one to ask, and the empty name
      --  makes creating the user fail, which Run_Init reports.

      if Console = Rose.Capabilities.Null_Capability then
         Init.Calls.Send_String
           (Output, "init: no console to ask for the first user on" & NL);
         return;
      end if;

      Rose.Interfaces.Screens.Terminal.Client.Open (Terminal, Console);

      Init.Calls.Send_String
        (Output, NL & "Create the first user." & NL);
      loop
         Init.Calls.Send_String (Output, "User name: ");
         Read_Line (Terminal, Name, Name_Last);
         exit when Valid_Name;
         Init.Calls.Send_String
           (Output,
            "A user name is from 1 to 64 letters, digits, '_', '-' and"
            & " '.'." & NL);
      end loop;

      loop
         declare
            Again      : String (1 .. Max_Password_Length);
            Again_Last : Natural;
         begin

            --  The line break that ends a password isn't echoed either, so
            --  it is written after each.

            Rose.Interfaces.Screens.Terminal.Client.Disable_Echo (Terminal);
            Init.Calls.Send_String (Output, "Password: ");
            Read_Line (Terminal, Password, Password_Last);
            Init.Calls.Send_String (Output, NL & "Password again: ");
            Read_Line (Terminal, Again, Again_Last);
            Init.Calls.Send_String (Output, NL);
            Rose.Interfaces.Screens.Terminal.Client.Enable_Echo (Terminal);

            exit when Password_Last >= Password'First
              and then Password (Password'First .. Password_Last)
                         = Again (Again'First .. Again_Last);

            Init.Calls.Send_String
              (Output,
               (if Password_Last < Password'First
                then "The password is empty."
                else "The passwords are not the same."));
            Init.Calls.Send_String (Output, NL);
         end;
      end loop;

      Forget (Terminal);
   end Ask_First_User;

   -----------------
   -- Check_Login --
   -----------------

   function Check_Login
     (Auth     : Rose.Capabilities.Capability;
      Name     : String;
      Password : String)
      return Boolean
   is
      Client : Rose.Interfaces.Users.Auth.Client.Auth_Client;
   begin
      Rose.Interfaces.Users.Auth.Client.Open (Client, Auth);
      return Rose.Interfaces.Users.User.Client.Get_Interface_Cap
        (Rose.Interfaces.Users.Auth.Client.Login (Client, Name, Password))
        /= Rose.Capabilities.Null_Capability;
   end Check_Login;

   ------------------
   -- Check_Rights --
   ------------------

   function Check_Rights
     (Rights : Rose.Capabilities.Capability;
      User   : Rose.Capabilities.Capability;
      Role   : String)
      return Boolean
   is
      Rights_Client : Rose.Interfaces.Users.Rights.Client.Rights_Client;
      User_Client   : Rose.Interfaces.Users.User.Client.User_Client;
      Grant         : Rose.Words.Word;
      Granted       : Rose.Capabilities.Capability;
      Refused       : Boolean;
   begin
      if Rights = Rose.Capabilities.Null_Capability
        or else User = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      Rose.Interfaces.Users.Rights.Client.Open (Rights_Client, Rights);
      Rose.Interfaces.Users.User.Client.Open (User_Client, User);

      --  An ordinary request has to be refused. If it isn't, end the
      --  grant, and fail.

      Rose.Interfaces.Users.Rights.Client.Request
        (Rights_Client, User_Client, Role, 0, Grant, Granted);
      Refused := Granted = Rose.Capabilities.Null_Capability;
      if not Refused then
         Rose.Interfaces.Users.Rights.Client.Finish
           (Rights_Client, User_Client, Grant);
         Rose.System_Calls.Delete_Cap (Granted);
      end if;

      --  A request under sudo has to be granted.

      Rose.Interfaces.Users.Rights.Client.Request
        (Rights_Client, User_Client, Role, 1, Grant, Granted);
      if Granted = Rose.Capabilities.Null_Capability then
         return False;
      end if;
      Rose.Interfaces.Users.Rights.Client.Finish
        (Rights_Client, User_Client, Grant);
      Rose.System_Calls.Delete_Cap (Granted);

      return Refused;
   end Check_Rights;

   -----------------------
   -- Create_First_User --
   -----------------------

   procedure Create_First_User
     (Name     : String;
      Password : String;
      Admin    : Rose.Capabilities.Capability;
      User     : out Rose.Capabilities.Capability;
      Home     : out Rose.Capabilities.Capability)
   is
      use Rose.Interfaces.Files.Writable_Directory.Client;
      package Files renames Rose.Interfaces.Files.Writable_File.Client;
      package Writers renames Rose.Interfaces.Streams.Releasable_Writer.Client;

      Admin_Client : Rose.Interfaces.Users.Auth_Admin.Client.Auth_Admin_Client;
      User_Client  : Rose.Interfaces.Users.User.Client.User_Client;
      Directory    : Writable_Directory_Client;
      Rc_File      : Files.Writable_File_Client;
      Rc_Writer    : Writers.Releasable_Writer_Client;
      Rc_Text      : Storage_Array (1 .. Petal_Rc'Length);
   begin
      User := Rose.Capabilities.Null_Capability;
      Home := Rose.Capabilities.Null_Capability;

      if Admin = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.Interfaces.Users.Auth_Admin.Client.Open (Admin_Client, Admin);
      User_Client :=
        Rose.Interfaces.Users.Auth_Admin.Client.New_User
          (Admin_Client, Name, Password);
      if Rose.Interfaces.Users.User.Client.Get_Interface_Cap (User_Client)
        = Rose.Capabilities.Null_Capability
      then
         return;
      end if;

      Directory := Rose.Interfaces.Users.User.Client
        .Home_Directory (User_Client);
      if Get_Interface_Cap (Directory) = Rose.Capabilities.Null_Capability
      then
         return;
      end if;

      Rc_File := Create_File (Directory, ".petalrc");
      if Files.Get_Interface_Cap (Rc_File)
        = Rose.Capabilities.Null_Capability
      then
         return;
      end if;

      for I in Petal_Rc'Range loop
         Rc_Text (Storage_Offset (I - Petal_Rc'First + 1)) :=
           Character'Pos (Petal_Rc (I));
      end loop;
      Rc_Writer := Files.Rewrite (Rc_File);
      Writers.Write (Rc_Writer, Rc_Text);
      Writers.Release (Rc_Writer);
      Writers.Close (Rc_Writer);

      User := Rose.Interfaces.Users.User.Client
        .Get_Interface_Cap (User_Client);
      Home := Get_Interface_Cap (Directory);
   end Create_First_User;

   -----------------------------
   -- Create_System_Directory --
   -----------------------------

   procedure Create_System_Directory
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability;
      Cap_Sets        : Rose.Capabilities.Capability;
      System          : out Rose.Capabilities.Capability)
   is
      use type Rose.Words.Word;

      Directory_Launch : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "directory");
      File_Launch      : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "file");
      Null_Launch      : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "null_stream");

      Fixed_Ancestor : constant := 3;
      --  Asked for its Nth ancestor, a Writable_Directory interface cap
      --  gives Tree for 1, Directory for 2 and Fixed_Directory for 3, as
      --  the generated clients ask for them.

      function Launch_Command
        (Program : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability;
      --  Launch Program as a command, against init's own bank, and return
      --  its command endpoint, or the null cap.

      --------------------
      -- Launch_Command --
      --------------------

      function Launch_Command
        (Program : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability
      is
         Launcher : Rose.Interfaces.Processes.Launch.Client.Launch_Client;
      begin
         Rose.Interfaces.Processes.Launch.Client.Open (Launcher, Program);
         return Rose.Interfaces.Processes.Launch.Client.Launch_Command
           (Launcher, Rose.Capabilities.Null_Capability);
      end Launch_Command;

      Params      : aliased Rose.Invocation.Invocation_Record;
      Endpoint    : Rose.Capabilities.Capability;
      Served      : Rose.Capabilities.Capability;
      Null_Cap    : Rose.Capabilities.Capability;
      Directory   : Rose.Interfaces.Files.Writable_Directory.Client
        .Writable_Directory_Client;
      Null_Stream : Rose.Interfaces.Streams.Stream.Client.Stream_Client;
      Linking        : Rose.Capabilities.Capability;
      Null_Dir_Cap   : Rose.Capabilities.Capability;
      Linker         : Rose.Interfaces.Files.Linking_Directory.Client
        .Linking_Directory_Client;
      Null_Directory : Rose.Interfaces.Files.Writable_Directory.Client
        .Writable_Directory_Client;
   begin
      System := Rose.Capabilities.Null_Capability;

      if Directory_Launch = Rose.Capabilities.Null_Capability
        or else File_Launch = Rose.Capabilities.Null_Capability
        or else Null_Launch = Rose.Capabilities.Null_Capability
      then
         return;
      end if;

      --  The directory command's roles' caps, as src/drivers/directory/
      --  rose-core-directory.idl declares them: Create_Endpoint,
      --  Delete_Cap, Cap_Set_Service, File_Launch and Directory_Launch,
      --  then Previous, which is null for a new directory; and its
      --  results Served and Control. Nothing holds its Control yet, so
      --  the system directory isn't replaced by an update.

      Endpoint := Launch_Command (Directory_Launch);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Caps
        (Params,
         (Create_Endpoint,
          Delete_Cap,
          Cap_Sets,
          File_Launch,
          Directory_Launch,
          Rose.Capabilities.Null_Capability));
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 3);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= Completed
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return;
      end if;

      Served := Params.Caps (0);
      Rose.System_Calls.Delete_Cap (Params.Caps (1));
      Linking := Params.Caps (2);

      --  The null stream command's role, Create_Endpoint, and its results
      --  Served and Null_Directory (src/drivers/null_stream/
      --  rose-core-null_stream.idl). It deletes its own caps with the
      --  Delete flag (#1022), so it has no Delete_Cap role (#1023).

      Endpoint := Launch_Command (Null_Launch);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= Completed
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return;
      end if;

      Null_Cap := Params.Caps (0);
      Null_Dir_Cap := Params.Caps (1);

      Rose.Interfaces.Files.Writable_Directory.Client.Open
        (Directory, Served);
      Rose.Interfaces.Streams.Stream.Client.Open (Null_Stream, Null_Cap);
      Rose.Interfaces.Files.Writable_Directory.Client.Add_Special_File
        (Directory, "null", Null_Stream);

      if Rose.Interfaces.Files.Writable_Directory.Client.Find_Entry
        (Directory, "null") = 0
      then
         return;
      end if;

      --  The null directory, as null-directory, through the right to
      --  link into the system directory that only init has (#1045).

      Rose.Interfaces.Files.Writable_Directory.Client.Open
        (Null_Directory, Null_Dir_Cap);
      Rose.Interfaces.Files.Linking_Directory.Client.Open (Linker, Linking);
      Rose.Interfaces.Files.Linking_Directory.Client.Add_Directory
        (Linker, "null-directory", Null_Directory);

      Rose.System_Calls.Initialize_Send (Params, Served);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Fixed_Ancestor));
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if not Params.Control.Flags (Rose.Invocation.Error) then
         System := Params.Caps (0);
      end if;
   end Create_System_Directory;

   ------------
   -- Forget --
   ------------

   procedure Forget
     (Console : in out Rose.Interfaces.Screens.Terminal.Client.Terminal_Client)
   is
      use Rose.Interfaces.Screens.Terminal.Client;
      Caps : constant Rose.Capabilities.Capability_Array :=
        (Get_Read_Cap (Console), Get_Write_Cap (Console),
         Get_Get_Size_Cap (Console), Get_Move_Cursor_Cap (Console),
         Get_Clear_Cap (Console), Get_Clear_To_End_Of_Line_Cap (Console),
         Get_Scroll_Cap (Console), Get_Set_Attributes_Cap (Console),
         Get_Read_Key_Cap (Console),
         Get_Disable_Line_Editing_Cap (Console),
         Get_Enable_Line_Editing_Cap (Console),
         Get_Enable_Echo_Cap (Console), Get_Disable_Echo_Cap (Console));
   begin
      for Cap of Caps loop
         if Cap /= Rose.Capabilities.Null_Capability then
            Rose.System_Calls.Delete_Cap (Cap);
         end if;
      end loop;
   end Forget;

   ---------------
   -- Read_Line --
   ---------------

   procedure Read_Line
     (Console : Rose.Interfaces.Screens.Terminal.Client.Terminal_Client;
      Line    : out String;
      Last    : out Natural)
   is
      Buffer : Storage_Array (1 .. 256);
      Count  : Storage_Offset;
   begin
      Last := Line'First - 1;
      loop
         Rose.Interfaces.Screens.Terminal.Client.Read (Console, Buffer, Count);
         exit when Count < Buffer'First;
         for Byte of Buffer (Buffer'First .. Count) loop
            if Byte = 10 then
               return;
            elsif Byte /= 13 and then Last < Line'Last then
               Last := Last + 1;
               Line (Last) := Character'Val (Byte);
            end if;
         end loop;
      end loop;
   end Read_Line;

   ----------------
   -- Start_Auth --
   ----------------

   procedure Start_Auth
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Cap_Sets        : Rose.Capabilities.Capability;
      Auth            : out Rose.Capabilities.Capability;
      Admin           : out Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability)
   is
      use type Rose.Words.Word;
      Launch_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "auth");
      Launcher   : Rose.Interfaces.Processes.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
   begin
      Auth := Rose.Capabilities.Null_Capability;
      Admin := Rose.Capabilities.Null_Capability;
      Control := Rose.Capabilities.Null_Capability;

      if Launch_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.Interfaces.Processes.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Processes.Launch.Client.Launch_Command
          (Launcher, Rose.Capabilities.Null_Capability);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return;
      end if;

      --  Its roles' caps, as src/utilities/auth/rose-core-auth.idl
      --  declares them: Create_Endpoint, Find_Command, then
      --  Cap_Set_Service; and its results Auth, Admin and Control.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Commands);
      Rose.System_Calls.Send_Cap (Params, Cap_Sets);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 3);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= Completed
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return;
      end if;

      Auth := Params.Caps (0);
      Admin := Params.Caps (1);
      Control := Params.Caps (2);
   end Start_Auth;

   -------------------
   -- Start_Console --
   -------------------

   procedure Start_Console (Control : Rose.Capabilities.Capability) is
      Supervisor : Rose.Interfaces.Screens.Supervisor.Client
        .Supervisor_Client;
   begin
      Rose.Interfaces.Screens.Supervisor.Client.Open (Supervisor, Control);
      Rose.Interfaces.Screens.Supervisor.Client.Start (Supervisor);
   end Start_Console;

   ------------------
   -- Start_Rights --
   ------------------

   procedure Start_Rights
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Console         : Rose.Capabilities.Capability;
      Clock_Admin     : Rose.Capabilities.Capability;
      Rights          : out Rose.Capabilities.Capability;
      Admin           : out Rose.Capabilities.Capability)
   is
      use type Rose.Words.Word;
      Launch_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "rights");
      Launcher   : Rose.Interfaces.Processes.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
   begin
      Rights := Rose.Capabilities.Null_Capability;
      Admin := Rose.Capabilities.Null_Capability;

      if Launch_Cap = Rose.Capabilities.Null_Capability
        or else Clock_Admin = Rose.Capabilities.Null_Capability
      then
         return;
      end if;

      Rose.Interfaces.Processes.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Processes.Launch.Client.Launch_Command
          (Launcher, Rose.Capabilities.Null_Capability);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return;
      end if;

      --  Its roles' caps, as src/utilities/rights/rose-core-rights.idl
      --  declares them: Create_Endpoint, Standard_Output, and
      --  Clock_Admin_Service.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Console);
      Rose.System_Calls.Send_Cap (Params, Clock_Admin);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= Completed
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return;
      end if;

      Rights := Params.Caps (0);
      Admin := Params.Caps (1);
   end Start_Rights;

   ----------------------
   -- Start_Supervisor --
   ----------------------

   function Start_Supervisor
     (Find_Cap        : Rose.Capabilities.Capability;
      Console         : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Timer           : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability;
      Rights          : Rose.Capabilities.Capability;
      System          : Rose.Capabilities.Capability;
      Auth            : Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      Launch_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "supervisor");
      Launcher   : Rose.Interfaces.Processes.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
   begin
      Control := Rose.Capabilities.Null_Capability;
      if Launch_Cap = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.Interfaces.Processes.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Processes.Launch.Client.Launch_Command
          (Launcher, Rose.Capabilities.Null_Capability);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  Its roles' caps and its Auth argument, as src/utilities/
      --  supervisor/rose-core-supervisor.idl declares them; and its
      --  result, Control.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Caps
        (Params,
         (Console,
          Create_Endpoint,
          Rescind_Cap,
          Commands,
          Timer,
          Clock,
          Rights,
          System,
          Auth));
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= Completed
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return False;
      end if;

      Control := Params.Caps (0);
      return True;
   end Start_Supervisor;

   --------------------
   -- Wait_For_Enter --
   --------------------

   procedure Wait_For_Enter
     (Console : Rose.Capabilities.Capability;
      Output  : Rose.Capabilities.Capability;
      Message : String)
   is
      Terminal : Rose.Interfaces.Screens.Terminal.Client.Terminal_Client;
      Line     : String (1 .. 80);
      Last     : Natural;
   begin
      Rose.Interfaces.Screens.Terminal.Client.Open (Terminal, Console);
      Init.Calls.Send_String (Output, Message);
      Read_Line (Terminal, Line, Last);
      Forget (Terminal);
   end Wait_For_Enter;

end Init.Users;
