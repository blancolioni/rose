with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;

with Rose.Interfaces.Auth.Client;
with Rose.Interfaces.Auth_Admin.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Rights.Client;
with Rose.Interfaces.Rights_Admin.Client;
with Rose.Interfaces.User.Client;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File.Client;

with Init.Calls;

package body Init.Users is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   Petal_Rc : constant String := "echo Welcome to Rose" & Character'Val (10);

   Completed : constant Rose.Words.Word := 0;

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
      Client : Rose.Interfaces.Rights_Admin.Client.Rights_Admin_Client;
   begin
      if Admin = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.Interfaces.Rights_Admin.Client.Open (Client, Admin);
      return Rose.Interfaces.Rights_Admin.Client.Allow
        (Client, User, Category, Mode) = 1;
   end Allow;

   -----------------
   -- Check_Login --
   -----------------

   function Check_Login
     (Auth     : Rose.Capabilities.Capability;
      Name     : String;
      Password : String)
      return Boolean
   is
      Client : Rose.Interfaces.Auth.Client.Auth_Client;
   begin
      Rose.Interfaces.Auth.Client.Open (Client, Auth);
      return Rose.Interfaces.User.Client.Get_Interface_Cap
        (Rose.Interfaces.Auth.Client.Login (Client, Name, Password))
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
      Rights_Client : Rose.Interfaces.Rights.Client.Rights_Client;
      User_Client   : Rose.Interfaces.User.Client.User_Client;
      Grant         : Rose.Words.Word;
      Granted       : Rose.Capabilities.Capability;
      Refused       : Boolean;
   begin
      if Rights = Rose.Capabilities.Null_Capability
        or else User = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      Rose.Interfaces.Rights.Client.Open (Rights_Client, Rights);
      Rose.Interfaces.User.Client.Open (User_Client, User);

      --  An ordinary request has to be refused. If it isn't, end the
      --  grant, and fail.

      Rose.Interfaces.Rights.Client.Request
        (Rights_Client, User_Client, Role, 0, Grant, Granted);
      Refused := Granted = Rose.Capabilities.Null_Capability;
      if not Refused then
         Rose.Interfaces.Rights.Client.Finish
           (Rights_Client, User_Client, Grant);
         Rose.System_Calls.Delete_Cap (Granted);
      end if;

      --  A request under sudo has to be granted.

      Rose.Interfaces.Rights.Client.Request
        (Rights_Client, User_Client, Role, 1, Grant, Granted);
      if Granted = Rose.Capabilities.Null_Capability then
         return False;
      end if;
      Rose.Interfaces.Rights.Client.Finish
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
      use Rose.Interfaces.Writable_Directory.Client;
      package Files renames Rose.Interfaces.Writable_File.Client;

      Admin_Client : Rose.Interfaces.Auth_Admin.Client.Auth_Admin_Client;
      User_Client  : Rose.Interfaces.User.Client.User_Client;
      Directory    : Writable_Directory_Client;
      Rc_File      : Files.Writable_File_Client;
      Rc_Text      : Storage_Array (1 .. Petal_Rc'Length);
   begin
      User := Rose.Capabilities.Null_Capability;
      Home := Rose.Capabilities.Null_Capability;

      if Admin = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.Interfaces.Auth_Admin.Client.Open (Admin_Client, Admin);
      User_Client :=
        Rose.Interfaces.Auth_Admin.Client.New_User
          (Admin_Client, Name, Password);
      if Rose.Interfaces.User.Client.Get_Interface_Cap (User_Client)
        = Rose.Capabilities.Null_Capability
      then
         return;
      end if;

      Directory := Rose.Interfaces.User.Client.Home_Directory (User_Client);
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
      Files.Write (Rc_File, Rc_Text);

      User := Rose.Interfaces.User.Client.Get_Interface_Cap (User_Client);
      Home := Get_Interface_Cap (Directory);
   end Create_First_User;

   ----------------
   -- Start_Auth --
   ----------------

   procedure Start_Auth
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Auth            : out Rose.Capabilities.Capability;
      Admin           : out Rose.Capabilities.Capability)
   is
      use type Rose.Words.Word;
      Launch_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "auth");
      No_Caps    : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher   : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
   begin
      Auth := Rose.Capabilities.Null_Capability;
      Admin := Rose.Capabilities.Null_Capability;

      if Launch_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.Interfaces.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command (Launcher, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return;
      end if;

      --  Its roles' caps, as src/utilities/auth/rose-core-auth.idl
      --  declares them: Create_Endpoint, then Find_Command.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Commands);
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

      Auth := Params.Caps (0);
      Admin := Params.Caps (1);
   end Start_Auth;

   -----------------
   -- Start_Login --
   -----------------

   function Start_Login
     (Find_Cap        : Rose.Capabilities.Capability;
      Console         : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Timer           : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability;
      Auth            : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      Launch_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "login");
      No_Caps    : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher   : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
   begin
      if Launch_Cap = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.Interfaces.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command (Launcher, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  Its roles' caps and its Auth argument, as
      --  src/utilities/login/rose-core-login.idl declares them.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Console);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Delete_Cap);
      Rose.System_Calls.Send_Cap (Params, Rescind_Cap);
      Rose.System_Calls.Send_Cap (Params, Commands);
      Rose.System_Calls.Send_Cap (Params, Timer);
      Rose.System_Calls.Send_Cap (Params, Clock);
      Rose.System_Calls.Send_Cap (Params, Auth);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      return not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
        and then Params.Data (0) = Completed;
   end Start_Login;

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
      No_Caps    : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher   : Rose.Interfaces.Launch.Client.Launch_Client;
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

      Rose.Interfaces.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command (Launcher, No_Caps);
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

end Init.Users;
