with Ada.Containers.Doubly_Linked_Lists;

with System.Storage_Elements;

with Rose.Capabilities.Standard;
with Rose.Crypto.PBKDF2_HMAC_SHA_256;
with Rose.Invocation;
with Rose.Objects;
with Rose.Random;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Auth.Server;
with Rose.Interfaces.Auth_Admin.Server;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Process.Client;

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
      end record;

   package User_Lists is
     new Ada.Containers.Doubly_Linked_Lists (User_Record);

   Users    : User_Lists.List;
   Context  : Rose.Server.Server_Context;
   Programs : Rose.Interfaces.Commands.Client.Commands_Client;

   Auth_Interface_Cap  : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Admin_Interface_Cap : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;

   function Login
     (Id       : Rose.Objects.Capability_Identifier;
      Name     : String;
      Password : String)
      return Rose.Capabilities.Capability;

   function New_User
     (Id       : Rose.Objects.Capability_Identifier;
      Name     : String;
      Password : String)
      return Rose.Capabilities.Capability;

   function Find (Name : String) return User_Lists.Cursor;

   procedure Hash_Password
     (Password   : String;
      Salt       : Storage_Array;
      Iterations : Positive;
      Hash       : out Storage_Array);

   function Launch_User
     (Name : String;
      Home : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;
   --  Launch the user program as a command with Name and Home (its
   --  declaration is src/utilities/user/rose-core-user.idl), and return
   --  the User interface from the reply, or null.

   ---------------
   -- Admin_Cap --
   ---------------

   function Admin_Cap return Rose.Capabilities.Capability is
   begin
      return Admin_Interface_Cap;
   end Admin_Cap;

   --------------
   -- Auth_Cap --
   --------------

   function Auth_Cap return Rose.Capabilities.Capability is
   begin
      return Auth_Interface_Cap;
   end Auth_Cap;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Commands : Rose.Interfaces.Commands.Client.Commands_Client)
   is
      Self : Rose.Interfaces.Process.Client.Process_Client;
   begin
      Programs := Commands;

      Rose.Interfaces.Auth.Server.Publish_Interface
        (Server_Context => Context,
         Login          => Login'Access);
      Rose.Interfaces.Auth_Admin.Server.Attach_Interface
        (Server_Context => Context,
         New_User       => New_User'Access);

      --  The generated server keeps the cap it publishes to itself, so
      --  ask the process for it. Auth_Admin is attached, and its
      --  interface endpoint has no cap until one is made here.

      Rose.Interfaces.Process.Client.Open
        (Self, Rose.Capabilities.Standard.Process_Interface_Cap);
      Auth_Interface_Cap :=
        Rose.Interfaces.Process.Client.Published_Interface (Self);
      Admin_Interface_Cap :=
        Rose.Server.Create_Endpoint
          (Rose.Interfaces.Auth_Admin.Auth_Admin_Interface);
   end Create_Server;

   ----------
   -- Find --
   ----------

   function Find (Name : String) return User_Lists.Cursor is
   begin
      for Position in Users.Iterate loop
         declare
            Item : User_Record renames Users (Position);
         begin
            if Item.Name (1 .. Item.Name_Last) = Name then
               return Position;
            end if;
         end;
      end loop;
      return User_Lists.No_Element;
   end Find;

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

   function Launch_User
     (Name : String;
      Home : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      use type Rose.Words.Word;
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : constant Rose.Interfaces.Launch.Client.Launch_Client :=
                   Rose.Interfaces.Commands.Client.Find (Programs, "user");
      Endpoint : constant Rose.Capabilities.Capability :=
                   Rose.Interfaces.Launch.Client.Launch_Command
                     (Launcher, No_Caps);
      Params   : aliased Rose.Invocation.Invocation_Record;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;

      --  The name is buffer item 1, in the same form as the items of
      --  an invocation (Rose.Command_Line.Launch).

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap
        (Params, Rose.System_Calls.Create_Endpoint_Capability);
      Rose.System_Calls.Send_Cap (Params, Home);
      Rose.System_Calls.Start_Items (Params);
      Rose.System_Calls.Send_Text_Item (Params, Name);
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

   -----------
   -- Login --
   -----------

   function Login
     (Id       : Rose.Objects.Capability_Identifier;
      Name     : String;
      Password : String)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      Position : constant User_Lists.Cursor := Find (Name);
      Hash     : Storage_Array (1 .. Hash_Length);
   begin
      if not User_Lists.Has_Element (Position) then

         --  Take as long as for a user who exists, so that the time does
         --  not tell which names do.

         Hash_Password
           (Password, (1 .. Salt_Length => 0), Iterations, Hash);
         return Rose.Capabilities.Null_Capability;
      end if;

      declare
         Item : User_Record renames Users (Position);
      begin
         case Item.Algorithm is
            when PBKDF2_HMAC_SHA_256 =>
               Hash_Password (Password, Item.Salt, Item.Iterations, Hash);
         end case;

         if Rose.Crypto.Equal (Hash, Item.Hash) then
            return Item.User;
         else
            return Rose.Capabilities.Null_Capability;
         end if;
      end;
   end Login;

   --------------
   -- New_User --
   --------------

   function New_User
     (Id       : Rose.Objects.Capability_Identifier;
      Name     : String;
      Password : String)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      No_Environment : Storage_Array (1 .. 0);
   begin
      if Name'Length = 0 or else Name'Length > Max_Name_Length
        or else User_Lists.Has_Element (Find (Name))
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      declare
         File_Launch      : constant Rose.Interfaces.Launch.Client
           .Launch_Client :=
             Rose.Interfaces.Commands.Client.Find (Programs, "file");
         Directory_Launch : constant Rose.Interfaces.Launch.Client
           .Launch_Client :=
             Rose.Interfaces.Commands.Client.Find (Programs, "directory");
         Process          : Rose.Interfaces.Process.Client.Process_Client;
         Home             : Rose.Capabilities.Capability;
         User             : Rose.Capabilities.Capability;
         Item             : User_Record;
      begin

         --  Name and Password are the request's buffer, read in place.
         --  Copy the name and hash the password before this process makes
         --  invocations of its own.

         Item.Name (1 .. Name'Length) := Name;
         Item.Name_Last := Name'Length;
         Item.Algorithm := PBKDF2_HMAC_SHA_256;
         Item.Iterations := Iterations;
         Rose.Random.Fill (Item.Salt);
         Hash_Password (Password, Item.Salt, Item.Iterations, Item.Hash);

         --  The home directory, which can create files and directories
         --  (ADR 0003).

         Process :=
           Rose.Interfaces.Launch.Client.Launch
             (Directory_Launch,
              (Rose.Interfaces.Launch.Client.Get_Interface_Cap
                 (File_Launch),
               Rose.Interfaces.Launch.Client.Get_Interface_Cap
                 (Directory_Launch)),
              No_Environment);
         Home := Rose.Interfaces.Process.Client.Published_Interface (Process);
         if Home = Rose.Capabilities.Null_Capability then
            return Rose.Capabilities.Null_Capability;
         end if;

         User := Launch_User (Item.Name (1 .. Item.Name_Last), Home);
         if User = Rose.Capabilities.Null_Capability then
            return Rose.Capabilities.Null_Capability;
         end if;

         Item.User := User;
         Users.Append (Item);

         return User;
      end;
   end New_User;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Auth_Server;
