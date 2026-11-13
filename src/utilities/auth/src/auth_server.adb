with Rose.System_Calls.Own_Bank;
with Ada.Containers.Indefinite_Ordered_Maps;

with System.Storage_Elements;

with Rose.Core.User_Client;
with Rose.Crypto.PBKDF2_HMAC_SHA_256;
with Rose.Random;
with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Auth.Server;
with Rose.Interfaces.Auth_Admin.Server;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Commands.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;
with Rose.Interfaces.User.Client;

package body Auth_Server is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   Max_Name_Length : constant := 64;
   Salt_Length     : constant := 16;
   Hash_Length     : constant := 32;

   Iterations      : constant := 100_000;
   --  PBKDF2's iterations for a new password: about a second for a login
   --  in QEMU, and much less on hardware. A record keeps the count it
   --  was hashed with, so this can change.

   type Hash_Algorithm is (PBKDF2_HMAC_SHA_256);
   --  A record keeps its algorithm, so that a stronger one, such as
   --  Argon2id, can be added later without losing the users.

   type User_Record is
      record
         Name       : String (1 .. Max_Name_Length);
         Name_Last  : Natural;
         Algorithm  : Hash_Algorithm;
         Iterations : Positive;
         Salt       : Storage_Array (1 .. Salt_Length);
         Hash       : Storage_Array (1 .. Hash_Length);
         User       : Rose.Capabilities.Capability;
         Control    : Rose.Capabilities.Capability;
         --  The Replaceable interface of the user's server, which only
         --  auth holds, for a new version of the user program (#831).
      end record;

   package User_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => User_Record);
   --  The users, by name (#490).

   subtype User_Class is Rose.Interfaces.User.User_Object'Class;

   --  One object serves Auth, and another Auth_Admin. A login or a new
   --  user that fails gives a User with the null cap, which init and
   --  login check for.

   type Auth_Object is
     new Rose.Interfaces.Auth.Server.Auth_Interface with null record;

   overriding function Auth_Cap
     (Item : in out Auth_Object)
      return Rose.Capabilities.Capability;

   overriding function Login
     (Item     : in out Auth_Object;
      Name     : String;
      Password : String)
      return User_Class;

   type Admin_Object is
     new Rose.Interfaces.Auth_Admin.Server.Auth_Admin_Interface
     with null record;

   overriding function Auth_Admin_Cap
     (Item : in out Admin_Object)
      return Rose.Capabilities.Capability;

   overriding function New_User
     (Item     : in out Admin_Object;
      Name     : String;
      Password : String)
      return User_Class;

   Users     : User_Maps.Map;
   Context   : Rose.Server.Server_Context;
   Programs  : Rose.Interfaces.Commands.Client.Commands_Client;
   Cap_Sets  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   --  The cap set server's Constructor, for the home directories.
   The_Auth  : aliased Auth_Object;
   The_Admin : aliased Admin_Object;

   function No_User return User_Class
   is (Rose.Interfaces.User.Client.From_Cap
         (Rose.Capabilities.Null_Capability));

   procedure Hash_Password
     (Password   : String;
      Salt       : Storage_Array;
      Iterations : Positive;
      Hash       : out Storage_Array);

   procedure Launch_User
     (Name    : String;
      Home    : Rose.Capabilities.Capability;
      Bank    : Rose.Capabilities.Capability;
      User    : out Rose.Capabilities.Capability;
      Control : out Rose.Capabilities.Capability);
   --  Launch the user program as a command with Name, Home and Bank (its
   --  declaration is src/utilities/user/rose-core-user.idl), against
   --  Bank, and return its User and Replaceable interfaces, or null caps
   --  if the launch fails.

   ---------------
   -- Admin_Cap --
   ---------------

   function Admin_Cap return Rose.Capabilities.Capability is
   begin
      return Rose.Interfaces.Auth_Admin.Server.Cap_Of (The_Admin);
   end Admin_Cap;

   --------------------
   -- Auth_Admin_Cap --
   --------------------

   overriding function Auth_Admin_Cap
     (Item : in out Admin_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Auth_Admin.Server.Cap_Of (Item));

   --------------
   -- Auth_Cap --
   --------------

   function Auth_Cap return Rose.Capabilities.Capability is
   begin
      return Rose.Interfaces.Auth.Server.Cap_Of (The_Auth);
   end Auth_Cap;

   --------------
   -- Auth_Cap --
   --------------

   overriding function Auth_Cap
     (Item : in out Auth_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Auth.Server.Cap_Of (Item));

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Commands        : Rose.Capabilities.Capability;
      Cap_Set_Service : Rose.Capabilities.Capability)
   is
   begin
      Rose.Interfaces.Commands.Client.Open (Programs, Commands);
      Cap_Sets := Cap_Set_Service;
      Rose.Interfaces.Auth.Server.Publish (Context, The_Auth'Access);
      Rose.Interfaces.Auth_Admin.Server.Attach (Context, The_Admin'Access);
   end Create_Server;

   -------------------
   -- Hash_Password --
   -------------------

   procedure Hash_Password
     (Password   : String;
      Salt       : Storage_Array;
      Iterations : Positive;
      Hash       : out Storage_Array)
   is
      Password_Bytes : Storage_Array (1 .. Password'Length);
      for Password_Bytes'Address use Password'Address;
      pragma Import (Ada, Password_Bytes);
   begin
      Rose.Crypto.PBKDF2_HMAC_SHA_256.Derive
        (Password_Bytes, Salt, Iterations, Hash);
   end Hash_Password;

   -----------------
   -- Launch_User --
   -----------------

   procedure Launch_User
     (Name    : String;
      Home    : Rose.Capabilities.Capability;
      Bank    : Rose.Capabilities.Capability;
      User    : out Rose.Capabilities.Capability;
      Control : out Rose.Capabilities.Capability)
   is
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
                   Programs.Find ("user");
      Endpoint : constant Rose.Capabilities.Capability :=
                   Launcher.Launch_Command (Bank, No_Caps);
   begin
      User := Rose.Capabilities.Null_Capability;
      Control := Rose.Capabilities.Null_Capability;
      if Endpoint = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.Core.User_Client.Call
        (Endpoint        => Endpoint,
         Create_Endpoint => Rose.System_Calls.Create_Endpoint_Capability,
         Cap_Set_Service => Cap_Sets,
         Name            => Name,
         Home            => Home,
         Bank            => Bank,
         User            => User,
         Control         => Control);
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         User := Rose.Capabilities.Null_Capability;
         Control := Rose.Capabilities.Null_Capability;
   end Launch_User;

   -----------
   -- Login --
   -----------

   overriding function Login
     (Item     : in out Auth_Object;
      Name     : String;
      Password : String)
      return User_Class
   is
      pragma Unreferenced (Item);
      Position : constant User_Maps.Cursor := Users.Find (Name);
      Hash     : Storage_Array (1 .. Hash_Length);
   begin
      if not User_Maps.Has_Element (Position) then

         --  Take as long as for a user who exists, so that the time does
         --  not tell which names do.

         Hash_Password
           (Password, (1 .. Salt_Length => 0), Iterations, Hash);
         return No_User;
      end if;

      declare
         Item : User_Record renames Users (Position);
      begin
         case Item.Algorithm is
            when PBKDF2_HMAC_SHA_256 =>
               Hash_Password (Password, Item.Salt, Item.Iterations, Hash);
         end case;

         if Rose.Crypto.Equal (Hash, Item.Hash) then
            return Rose.Interfaces.User.Client.From_Cap (Item.User);
         else
            return No_User;
         end if;
      end;
   end Login;

   --------------
   -- New_User --
   --------------

   overriding function New_User
     (Item     : in out Admin_Object;
      Name     : String;
      Password : String)
      return User_Class
   is
      pragma Unreferenced (Item);
      New_Item  : User_Record;
      Home      : Rose.Capabilities.Capability;
      User      : Rose.Capabilities.Capability;
      Control   : Rose.Capabilities.Capability;
      User_Bank : Rose.Interfaces.Bank.Client.Bank_Client;

      function Give_Up return User_Class;
      --  Destroy the user's bank, which ends whatever was launched
      --  against it, and return No_User.

      -------------
      -- Give_Up --
      -------------

      function Give_Up return User_Class is
      begin
         begin
            User_Bank.Destroy;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;
         return No_User;
      end Give_Up;

   begin
      if Name'Length = 0 or else Name'Length > Max_Name_Length
        or else Users.Contains (Name)
      then
         return No_User;
      end if;

      --  Name and Password are the request's buffer, read in place.
      --  Copy the name and hash the password before this process makes
      --  invocations of its own.

      New_Item.Name (1 .. Name'Length) := Name;
      New_Item.Name_Last := Name'Length;
      New_Item.Algorithm := PBKDF2_HMAC_SHA_256;
      New_Item.Iterations := Iterations;
      Rose.Random.Fill (New_Item.Salt);
      Hash_Password
        (Password, New_Item.Salt, New_Item.Iterations, New_Item.Hash);

      --  The user's bank (ADR 0014, section 2; #450): the user's home
      --  directory and user server are launched against it, and each of
      --  the user's sessions is a sub-bank of it. It is a sub-bank of
      --  auth's own bank, which is in the system bank.

      declare
         Own : Rose.Interfaces.Bank.Client.Bank_Client :=
                 Rose.Interfaces.Bank.Client.From_Cap
                   (Rose.System_Calls.Own_Bank.Cap);
         Sub : Rose.Interfaces.Bank.Bank_Object'Class :=
                 Own.Create_Sub_Bank (0, 0);
      begin
         User_Bank.Open (Sub.Bank_Cap);
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return No_User;
      end;

      --  The home directory, which can create files and directories
      --  (ADR 0003). A program that can't be found raises.

      begin
         declare
            File_Launch      : Rose.Interfaces.Launch.Launch_Object'Class :=
              Programs.Find ("file");
            Directory_Launch : Rose.Interfaces.Launch.Launch_Object'Class :=
              Programs.Find ("directory");
            Process          : Rose.Interfaces.Process.Process_Object'Class :=
              Directory_Launch.Launch
                (User_Bank.Get_Interface_Cap,
                 (File_Launch.Launch_Cap, Directory_Launch.Launch_Cap,
                  Cap_Sets));
         begin

            --  Elf answers a launch that fails with the null cap (#724).

            if Process.Process_Cap = Rose.Capabilities.Null_Capability then
               return Give_Up;
            end if;
            Home := Process.Published_Interface;
         end;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Give_Up;
      end;

      if Home = Rose.Capabilities.Null_Capability then
         return Give_Up;
      end if;

      Launch_User
        (New_Item.Name (1 .. New_Item.Name_Last), Home,
         User_Bank.Get_Interface_Cap, User, Control);
      if User = Rose.Capabilities.Null_Capability
        or else Control = Rose.Capabilities.Null_Capability
      then
         return Give_Up;
      end if;

      New_Item.User := User;
      New_Item.Control := Control;
      Users.Insert (New_Item.Name (1 .. New_Item.Name_Last), New_Item);

      return Rose.Interfaces.User.Client.From_Cap (User);
   end New_User;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Auth_Server;
