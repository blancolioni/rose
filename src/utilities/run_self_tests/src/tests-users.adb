with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Launch;
with Rose.Interfaces.User.Client;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File;

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
      User_Name   : constant String := "test";
      No_Caps     : Rose.Capabilities.Capability_Array (1 .. 0);
      User_Launch : Rose.Interfaces.Launch.Launch_Object'Class :=
        Tests.Registry.Launcher ("user");
      Home        : Rose.Interfaces.Writable_Directory.Client
        .Writable_Directory_Client;
      Endpoint    : Rose.Capabilities.Capability;
      Params      : aliased Rose.Invocation.Invocation_Record;
      User        : Rose.Interfaces.User.Client.User_Client;
   begin
      Ada.Text_IO.Put ("User test");

      Home.Open
        (Tests.Registry.Launch
           ("directory",
            (Tests.Registry.Launcher_Cap ("file"),
             Tests.Registry.Launcher_Cap ("directory"))));

      --  The user is launched as a command, and its launch call gives it
      --  a create endpoint cap, its home directory and its name, as one
      --  buffer item: the length, little-endian, then the text, padded
      --  to four bytes.

      Endpoint := User_Launch.Launch_Command (No_Caps);
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
         Tests.Registry.Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Home.Get_Interface_Cap);
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

      User.Open (Params.Caps (0));

      if User.Name /= User_Name then
         return Fail ("wrong name");
      end if;

      --  A file created through the user's home directory is in the
      --  directory that the user was given.

      declare
         User_Home : Rose.Interfaces.Writable_Directory
           .Writable_Directory_Object'Class :=
             User.Home_Directory;
         Created   : Rose.Interfaces.Writable_File
           .Writable_File_Object'Class :=
             User_Home.Create_File (".petalrc");
      begin
         if Created.Writable_File_Cap = Rose.Capabilities.Null_Capability
         then
            return Fail ("cannot create a file in the home directory");
         end if;
      end;

      if Home.Directory_Entry_Count /= 1
        or else Home.Find_Entry (".petalrc") /= 1
      then
         return Fail ("the home directory is not the one given");
      end if;

      return True;
   end User_Test;

end Tests.Users;
