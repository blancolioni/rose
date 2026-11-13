with Ada.Text_IO;

with Rose.Core.User_Client;
with Rose.Server;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;

with Rose.Interfaces.Bank;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Commands.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;
with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.User.Client;
with Rose.Interfaces.Writable_Directory;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File;

package body Home_Holder is

   use type Rose.Capabilities.Capability;

   type Holder_Object is
     limited new Rose.Interfaces.Restorable.Server.Restorable_Interface
   with null record;

   overriding function Restorable_Cap
     (Item : in out Holder_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Restorable.Server.Cap_Of (Item));

   overriding procedure Restored (Item : in out Holder_Object);

   User_Name : constant String := "hold";
   File_Name : constant String := "before";

   Context    : Rose.Server.Server_Context;
   The_Holder : aliased Holder_Object;

   User    : Rose.Interfaces.User.Client.User_Client;
   Session : Rose.Interfaces.Bank.Client.Bank_Client;
   --  The user, and a session that is open across the handover and the
   --  restore.

   Checked : Boolean := False;
   --  Set once the user has been checked after a restore. The check makes
   --  sessions, so it is made once; the variable is saved with the rest of
   --  the process. It only calls servers that were restored: launching a
   --  process then, as creating a file does, hung the restored system in
   --  its first checkpoint.

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Commands        : Rose.Capabilities.Capability;
      Cap_Set_Service : Rose.Capabilities.Capability;
      User_Control    : out Rose.Capabilities.Capability)
   is
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Programs : Rose.Interfaces.Commands.Client.Commands_Client;
      Own      : Rose.Interfaces.Bank.Client.Bank_Client;
      Home     : Rose.Interfaces.Writable_Directory.Client
        .Writable_Directory_Client;
      Served   : Rose.Capabilities.Capability;
   begin
      User_Control := Rose.Capabilities.Null_Capability;
      Rose.Interfaces.Restorable.Server.Attach (Context, The_Holder'Access);
      Programs.Open (Commands);
      Own.Open (Rose.System_Calls.Own_Bank.Cap);

      declare
         User_Bank        : Rose.Interfaces.Bank.Bank_Object'Class :=
           Own.Create_Sub_Bank (0, 0);
         --  The user's bank, as auth makes one, which the home directory
         --  and the user server are launched against.
         File_Launch      : Rose.Interfaces.Launch.Launch_Object'Class :=
           Programs.Find ("file");
         Directory_Launch : Rose.Interfaces.Launch.Launch_Object'Class :=
           Programs.Find ("directory");
         User_Launch      : Rose.Interfaces.Launch.Launch_Object'Class :=
           Programs.Find ("user");
         Process          : Rose.Interfaces.Process.Process_Object'Class :=
           Directory_Launch.Launch
             (User_Bank.Bank_Cap,
              (File_Launch.Launch_Cap, Directory_Launch.Launch_Cap,
               Cap_Set_Service));
         Endpoint         : Rose.Capabilities.Capability;
      begin
         if Process.Process_Cap = Rose.Capabilities.Null_Capability then
            raise Rose.Interfaces.Objects.Invocation_Error
              with "no home directory";
         end if;
         Home.Open (Process.Published_Interface);

         declare
            Created : Rose.Interfaces.Writable_File
              .Writable_File_Object'Class := Home.Create_File (File_Name);
         begin
            if Created.Writable_File_Cap = Rose.Capabilities.Null_Capability
            then
               raise Rose.Interfaces.Objects.Invocation_Error
                 with "no file in the home directory";
            end if;
         end;

         Endpoint :=
           User_Launch.Launch_Command (User_Bank.Bank_Cap, No_Caps);
         Rose.Core.User_Client.Call
           (Endpoint        => Endpoint,
            Create_Endpoint => Rose.System_Calls.Create_Endpoint_Capability,
            Cap_Set_Service => Cap_Set_Service,
            Name            => User_Name,
            Home            => Home.Get_Interface_Cap,
            Bank            => User_Bank.Bank_Cap,
            User            => Served,
            Control         => User_Control);
         User.Open (Served);

         declare
            Started : Rose.Interfaces.Bank.Bank_Object'Class :=
              User.New_Session;
         begin
            Session.Open (Started.Bank_Cap);
         end;
      end;

      Ada.Text_IO.Put_Line
        ("hold_home: holding a user with a home directory and a session");
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Ada.Text_IO.Put_Line ("hold_home: FAIL: the user did not start");
         User_Control := Rose.Capabilities.Null_Capability;
   end Create_Server;

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Holder_Object) is
      pragma Unreferenced (Item);
   begin
      if Checked then
         return;
      end if;
      Checked := True;

      declare
         Name : constant String := User.Name;
         Home : Rose.Interfaces.Writable_Directory
           .Writable_Directory_Object'Class := User.Home_Directory;
         Kept : constant Boolean := Home.Find_Entry (File_Name) = 1;
         Next : Rose.Interfaces.Bank.Bank_Object'Class := User.New_Session;
         Old  : Rose.Interfaces.Bank.Bank_Object'Class :=
           Session.Create_Sub_Bank (0, 0);
      begin
         if Name = User_Name
           and then Kept
           and then Next.Bank_Cap /= Rose.Capabilities.Null_Capability
           and then Old.Bank_Cap /= Rose.Capabilities.Null_Capability
         then
            Ada.Text_IO.Put_Line
              ("hold_home: restored: the replaced user has its name, "
               & "home directory and sessions");
         else
            Ada.Text_IO.Put_Line
              ("hold_home: FAIL: restored: name """ & Name & """"
               & (if Kept then "" else ", no file in the home directory")
               & (if Next.Bank_Cap = Rose.Capabilities.Null_Capability
                  then ", no new session" else "")
               & (if Old.Bank_Cap = Rose.Capabilities.Null_Capability
                  then ", the old session is gone" else ""));
         end if;
         Old.Destroy;
         Next.Destroy;
      end;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Ada.Text_IO.Put_Line
           ("hold_home: FAIL: restored: a call to the user failed");
   end Restored;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Home_Holder;
