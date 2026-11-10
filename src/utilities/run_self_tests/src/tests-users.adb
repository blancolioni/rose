with Rose.System_Calls.Own_Bank;
with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Ada.Streams;

with Rose.Interfaces.Bank;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.File_Control.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;
with Rose.Interfaces.User.Client;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File;
with Rose.Interfaces.Writable_File.Client;

with Tests.Files;
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

   -----------------
   -- Create_User --
   -----------------

   function Create_User
     (Name : String;
      Home : Rose.Capabilities.Capability;
      Bank : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
      return Rose.Capabilities.Capability
   is
      User_Launch : Rose.Interfaces.Launch.Launch_Object'Class :=
                      Tests.Registry.Launcher ("user");
      No_Caps     : Rose.Capabilities.Capability_Array (1 .. 0);
      Padded      : constant Storage_Count :=
                      Storage_Count ((Name'Length + 3) / 4 * 4);
      User_Bank   : constant Rose.Capabilities.Capability :=
                      (if Bank = Rose.Capabilities.Null_Capability
                       then Rose.System_Calls.Own_Bank.Cap
                       else Bank);
      Endpoint    : constant Rose.Capabilities.Capability :=
                      User_Launch.Launch_Command (User_Bank, No_Caps);
      Params      : aliased Rose.Invocation.Invocation_Record;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;

      --  The name is one buffer item: its length, little-endian, then
      --  the text, padded to four bytes.

      Name_Buffer (1 .. 4 + Padded) := (others => 0);
      Name_Buffer (1) := Storage_Element (Name'Length);
      for Index in Name'Range loop
         Name_Buffer (4 + Storage_Offset (Index - Name'First + 1)) :=
           Character'Pos (Name (Index));
      end loop;

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Tests.Registry.Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Home);
      Rose.System_Calls.Send_Cap (Params, User_Bank);
      Rose.System_Calls.Send_Buffer
        (Params, 4 + Padded, Name_Buffer'Address, False);
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
   end Create_User;

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Message);
      return False;
   end Fail;

   ------------------
   -- Session_Test --
   ------------------

   function Session_Test return Boolean is
      use Ada.Streams;
      use type Rose.Words.Word_64;
      package Files renames Rose.Interfaces.Writable_File.Client;
      Hello   : constant Stream_Element_Array (1 .. 5) :=
                  (Character'Pos ('h'), Character'Pos ('e'),
                   Character'Pos ('l'), Character'Pos ('l'),
                   Character'Pos ('o'));
      No_Caps : Rose.Capabilities.Capability_Array (1 .. 0);
      Own     : Rose.Interfaces.Bank.Client.Bank_Client :=
                  Rose.Interfaces.Bank.Client.From_Cap
                    (Rose.System_Calls.Own_Bank.Cap);
      User_Bank : Rose.Interfaces.Bank.Bank_Object'Class :=
                    Own.Create_Sub_Bank (0, 0);

      function Check return Boolean;
      --  The test itself, so that the user's bank is destroyed however
      --  it ends.

      -----------
      -- Check --
      -----------

      function Check return Boolean is
         Home : Rose.Interfaces.Writable_Directory.Client
           .Writable_Directory_Client;
         User : Rose.Interfaces.User.Client.User_Client;
         Before_Pages, Pages, Page_Limit : Rose.Words.Word_64;
         Before_Regions, Regions, Region_Limit : Rose.Words.Word;
      begin
         Home.Open
           (Tests.Registry.Launch
              ("directory",
               (Tests.Registry.Launcher_Cap ("file"),
                Tests.Registry.Launcher_Cap ("directory"))));
         User.Open
           (Create_User
              ("session", Home.Get_Interface_Cap, User_Bank.Bank_Cap));
         if User.User_Cap = Rose.Capabilities.Null_Capability then
            return Fail ("cannot launch a user");
         end if;

         User_Bank.Get_Usage
           (Before_Pages, Before_Regions, Page_Limit, Region_Limit);

         declare
            Session  : Rose.Interfaces.Bank.Bank_Object'Class :=
                         User.New_Session;
            Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
                         Tests.Registry.Launcher ("file");
            Server   : Rose.Interfaces.Process.Process_Object'Class :=
                         Launcher.Launch (Session.Bank_Cap, No_Caps);
            Control  : Rose.Interfaces.File_Control.Client
              .File_Control_Client;
            File     : Files.Writable_File_Client;
         begin
            Control.Open (Server.Published_Interface);
            declare
               Writable : Rose.Interfaces.Writable_File
                 .Writable_File_Object'Class := Control.Writable;
            begin
               File.Open (Writable.Writable_File_Cap);
            end;
            Tests.Files.Append (File, Hello);
            if File.Size /= Hello'Length then
               return Fail ("the file doesn't work");
            end if;

            User_Bank.Get_Usage (Pages, Regions, Page_Limit, Region_Limit);
            if Regions <= Before_Regions then
               return Fail ("the session's storage is not in the user's");
            end if;

            --  Ending the session destroys its bank, which ends the
            --  server without asking it.

            Session.Destroy;

            begin
               Ada.Text_IO.Put (Storage_Count'Image (File.Size));
               return Fail ("the file answers after the session ended");
            exception
               when Rose.Interfaces.Objects.Invocation_Error =>
                  null;
            end;
         end;

         User_Bank.Get_Usage (Pages, Regions, Page_Limit, Region_Limit);
         if Pages /= Before_Pages or else Regions /= Before_Regions then
            Ada.Text_IO.Put
              (Rose.Words.Word_64'Image (Before_Pages)
               & Rose.Words.Word'Image (Before_Regions)
               & Rose.Words.Word_64'Image (Pages)
               & Rose.Words.Word'Image (Regions));
            return Fail ("the session's storage was not released");
         end if;

         return True;
      end Check;

      Result : Boolean;
   begin
      Ada.Text_IO.Put ("Session test");
      Result := Check;

      --  The user's bank ends the user and its home directory.

      User_Bank.Destroy;
      return Result;
   end Session_Test;

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

      Endpoint := User_Launch.Launch_Command
                        (Rose.System_Calls.Own_Bank.Cap, No_Caps);
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
      Rose.System_Calls.Send_Cap (Params, Rose.System_Calls.Own_Bank.Cap);
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
