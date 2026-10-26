with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.User.Client;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File.Client;

with Tests.Registry;

package body Tests.Users is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word;

   Name_Buffer : Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  The buffer of the launch call, which holds the user's name.

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Message);
      return False;
   end Fail;

   ---------------
   -- User_Test --
   ---------------

   function User_Test return Boolean is
      use Rose.Interfaces.Writable_Directory.Client;
      package Users renames Rose.Interfaces.User.Client;
      package Writable_Files renames Rose.Interfaces.Writable_File.Client;

      File_Launch : constant Rose.Interfaces.Launch.Client.Launch_Client :=
        Tests.Registry.Launcher ("file");
      Directory_Launch : constant
        Rose.Interfaces.Launch.Client.Launch_Client :=
          Tests.Registry.Launcher ("directory");
      User_Launch : constant Rose.Interfaces.Launch.Client.Launch_Client :=
        Tests.Registry.Launcher ("user");
      User_Name   : constant String := "test";
      No_Caps     : Rose.Capabilities.Capability_Array (1 .. 0);
      No_Words    : Storage_Array (1 .. 0);
      Process     : Rose.Interfaces.Process.Client.Process_Client;
      Home        : Writable_Directory_Client;
      Endpoint    : Rose.Capabilities.Capability;
      Params      : aliased Rose.Invocation.Invocation_Record;
      User        : Users.User_Client;
      User_Home   : Writable_Directory_Client;
      Created     : Writable_Files.Writable_File_Client;
      Name        : String (1 .. 16);
      Name_Last   : Natural;
   begin
      Ada.Text_IO.Put ("User test");

      Process :=
        Rose.Interfaces.Launch.Client.Launch
          (Directory_Launch,
           (Rose.Interfaces.Launch.Client.Get_Interface_Cap (File_Launch),
            Rose.Interfaces.Launch.Client.Get_Interface_Cap
              (Directory_Launch)),
           No_Words);
      Open (Home, Rose.Interfaces.Process.Client.Published_Interface
                    (Process));

      --  The user is launched as a command, and its launch call gives it
      --  a create endpoint cap, its home directory and its name, as one
      --  buffer item: the length, little-endian, then the text, padded
      --  to four bytes.

      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command (User_Launch, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Fail ("cannot launch a user");
      end if;

      Name_Buffer (1 .. 8) :=
        (Storage_Element (User_Name'Length), 0, 0, 0,
         Character'Pos (User_Name (1)), Character'Pos (User_Name (2)),
         Character'Pos (User_Name (3)), Character'Pos (User_Name (4)));

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap
        (Params,
         Tests.Registry.Argument_Cap (Tests.Registry.Create_Endpoint_Index));
      Rose.System_Calls.Send_Cap (Params, Get_Interface_Cap (Home));
      Rose.System_Calls.Send_Buffer (Params, 8, Name_Buffer'Address, False);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= 0
      then
         return Fail ("the launch call failed");
      elsif not Params.Control.Flags (Rose.Invocation.Send_Caps)
        or else Params.Caps (0) = Rose.Capabilities.Null_Capability
      then
         return Fail ("no user interface in the reply");
      end if;

      Users.Open (User, Params.Caps (0));

      Users.Name (User, Name, Name_Last);
      if Name (1 .. Name_Last) /= User_Name then
         return Fail ("wrong name");
      end if;

      --  A file created through the user's home directory is in the
      --  directory that the user was given.

      User_Home := Users.Home_Directory (User);
      Created := Create_File (User_Home, ".petalrc");
      if Writable_Files.Get_Interface_Cap (Created)
        = Rose.Capabilities.Null_Capability
      then
         return Fail ("cannot create a file in the home directory");
      end if;

      if Directory_Entry_Count (Home) /= 1
        or else Find_Entry (Home, ".petalrc") /= 1
      then
         return Fail ("the home directory is not the one given");
      end if;

      return True;
   end User_Test;

end Tests.Users;
