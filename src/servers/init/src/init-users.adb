with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File.Client;

with Init.Calls;

package body Init.Users is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   Petal_Rc : constant String := "echo Welcome to Rose" & Character'Val (10);
   --  The contents of the new user's .petalrc.

   Max_Name_Length : constant := 64;
   --  The longest user name (the user utility's User_Server).

   function Launch_User
     (Launcher        : Rose.Interfaces.Launch.Client.Launch_Client;
      Create_Endpoint : Rose.Capabilities.Capability;
      Home            : Rose.Capabilities.Capability;
      Name            : String)
      return Rose.Capabilities.Capability;
   --  Launch the user program as a command, and call it with
   --  Create_Endpoint, Home and Name (see its command declaration,
   --  src/utilities/user/rose-core-user.idl). Return the User interface
   --  from the reply, or null if the launch or the call fails.

   -----------------------
   -- Create_First_User --
   -----------------------

   procedure Create_First_User
     (Name            : String;
      Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      User            : out Rose.Capabilities.Capability;
      Home            : out Rose.Capabilities.Capability)
   is
      use Rose.Interfaces.Launch.Client;
      use Rose.Interfaces.Writable_Directory.Client;
      package Files renames Rose.Interfaces.Writable_File.Client;

      File_Cap         : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "file");
      Directory_Cap    : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "directory");
      User_Cap         : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "user");
      No_Environment   : Storage_Array (1 .. 0);
      Directory_Launch : Launch_Client;
      User_Launch      : Launch_Client;
      Process          : Rose.Interfaces.Process.Client.Process_Client;
      Home_Cap         : Rose.Capabilities.Capability;
      Directory        : Writable_Directory_Client;
      Rc_File          : Files.Writable_File_Client;
      Rc_Text          : Storage_Array (1 .. Petal_Rc'Length);
   begin
      User := Rose.Capabilities.Null_Capability;
      Home := Rose.Capabilities.Null_Capability;

      if File_Cap = Rose.Capabilities.Null_Capability
        or else Directory_Cap = Rose.Capabilities.Null_Capability
        or else User_Cap = Rose.Capabilities.Null_Capability
      then
         return;
      end if;

      Open (Directory_Launch, Directory_Cap);
      Open (User_Launch, User_Cap);

      --  The home directory, which can create files and directories.

      Process :=
        Launch (Directory_Launch, (File_Cap, Directory_Cap), No_Environment);
      Home_Cap :=
        Rose.Interfaces.Process.Client.Published_Interface (Process);
      if Home_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;
      Open (Directory, Home_Cap);

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

      User := Launch_User
        (User_Launch, Create_Endpoint, Home_Cap, Name);
      if User /= Rose.Capabilities.Null_Capability then
         Home := Home_Cap;
      end if;
   end Create_First_User;

   -----------------
   -- Launch_User --
   -----------------

   function Launch_User
     (Launcher        : Rose.Interfaces.Launch.Client.Launch_Client;
      Create_Endpoint : Rose.Capabilities.Capability;
      Home            : Rose.Capabilities.Capability;
      Name            : String)
      return Rose.Capabilities.Capability
   is
      use type Rose.Words.Word;
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Endpoint : constant Rose.Capabilities.Capability :=
        Rose.Interfaces.Launch.Client.Launch_Command (Launcher, No_Caps);
      Length   : constant Storage_Count := Name'Length;
      Size     : constant Storage_Count := 4 + (Length + 3) / 4 * 4;
      Rest     : Storage_Count := Length;
      Item     : Storage_Array (1 .. 4 + Max_Name_Length) :=
        (others => 0);
      Params   : aliased Rose.Invocation.Invocation_Record;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability
        or else Name'Length > Max_Name_Length
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      --  The name is buffer item 1: its length, little-endian, then its
      --  text, padded to four bytes (Rose.Command_Line.Launch). It is
      --  sent through the invoke buffer that init already has, since a
      --  page of its own would not fit in the kernel heap that boot
      --  modules share before mem starts.

      for I in Storage_Offset range 1 .. 4 loop
         Item (I) := Storage_Element (Rest mod 256);
         Rest := Rest / 256;
      end loop;
      for I in Name'Range loop
         Item (5 + Storage_Offset (I - Name'First)) :=
           Character'Pos (Name (I));
      end loop;

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Home);
      Rose.System_Calls.Send_Storage_Array (Params, Item (1 .. Size), False);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= 0
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Params.Caps (0);
   end Launch_User;

end Init.Users;
