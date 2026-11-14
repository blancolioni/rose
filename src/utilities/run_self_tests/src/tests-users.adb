with Rose.System_Calls.Own_Bank;
with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Core.User_Client;
with Rose.System_Calls;
with Rose.Words;

with Ada.Streams;

with Rose.Interfaces.Bank;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Objects;
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
      No_Caps     : Rose.Capabilities.Capability_Array (1 .. 0);
      User_Launch : Rose.Interfaces.Launch.Launch_Object'Class :=
                      Tests.Registry.Launcher ("user");
      User_Bank   : constant Rose.Capabilities.Capability :=
                      (if Bank = Rose.Capabilities.Null_Capability
                       then Rose.System_Calls.Own_Bank.Cap
                       else Bank);
      Endpoint    : constant Rose.Capabilities.Capability :=
                      User_Launch.Launch_Command (User_Bank, No_Caps);
      User        : Rose.Capabilities.Capability;
      Control     : Rose.Capabilities.Capability;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;

      Start_User (Endpoint, Name, Home, User_Bank, User, Control);
      return User;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return Rose.Capabilities.Null_Capability;
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
           (Tests.Registry.Launch_Directory);
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
            File     : Files.Writable_File_Client;
         begin
            File.Open (Tests.Registry.Launch_File (Session.Bank_Cap));
            if File.Get_Interface_Cap = Rose.Capabilities.Null_Capability
            then
               return Fail ("no file server");
            end if;
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

   ----------------
   -- Start_User --
   ----------------

   procedure Start_User
     (Endpoint : Rose.Capabilities.Capability;
      Name     : String;
      Home     : Rose.Capabilities.Capability;
      Bank     : Rose.Capabilities.Capability;
      User     : out Rose.Capabilities.Capability;
      Control  : out Rose.Capabilities.Capability;
      Previous : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
   is
      Fresh : constant Boolean :=
        Previous = Rose.Capabilities.Null_Capability;

      function Given
        (Cap : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability
      is (if Fresh then Cap else Rose.Capabilities.Null_Capability);
      --  A new version takes its caps from the old one (#901), so a
      --  handover passes Previous alone.

   begin
      Rose.Core.User_Client.Call
        (Endpoint        => Endpoint,
         Create_Endpoint => Given (Tests.Registry.Create_Endpoint),
         Cap_Set_Service => Given (Tests.Registry.Cap_Sets),
         Name            => (if Fresh then Name else ""),
         Home            => Given (Home),
         Bank            => Given (Bank),
         User            => User,
         Control         => Control,
         Previous        => Previous);
   end Start_User;

   ---------------
   -- User_Test --
   ---------------

   function User_Test return Boolean is
      User_Name : constant String := "test";
      Home      : Rose.Interfaces.Writable_Directory.Client
        .Writable_Directory_Client;
      User      : Rose.Interfaces.User.Client.User_Client;
   begin
      Ada.Text_IO.Put ("User test");

      Home.Open
        (Tests.Registry.Launch_Directory);

      --  The user is launched as a command, whose launch call gives it a
      --  create endpoint cap, the cap set server, its home directory, its
      --  bank, and its name.

      User.Open (Create_User (User_Name, Home.Get_Interface_Cap));
      if User.User_Cap = Rose.Capabilities.Null_Capability then
         return Fail ("cannot launch a user");
      end if;

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
