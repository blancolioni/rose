with Ada.Text_IO;

with Rose.Capabilities.Standard;
with Rose.Objects;
with Rose.Server;
with Rose.Words;

with Rose.Interfaces.Clock_Admin.Server;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.Rights.Server;
with Rose.Interfaces.Rights_Admin.Server;
with Rose.Interfaces.User.Client;

with Rights_Policy;

package body Rights_Server is

   use type Rose.Capabilities.Capability;
   use type Rose.Objects.Capability_Identifier;
   use type Rose.Words.Word;

   package Clock_Admin_Client renames Rose.Interfaces.Clock_Admin.Client;
   package Clock_Admin_Server renames Rose.Interfaces.Clock_Admin.Server;

   --  The roles that the broker holds are the ones that it declares
   --  (rose-core-rights.idl), each with the proxy that a grant of it is,
   --  and a category (ADR 0021, section 1). The category is in the
   --  role's declaration (interfaces/clock_admin_service.idl), where
   --  petal reads it from the command's metadata. That metadata is a
   --  section of the executable that isn't loaded into the process, so
   --  the broker has its own copy, here. The broker's copy is the one
   --  that decides.

   Clock_Role     : constant String := "Clock_Admin_Service";
   Clock_Category : constant String := "time";

   --  A grant is an instance of a proxy for the role's interface, which
   --  the broker serves and forwards to the cap that it holds. There is
   --  no derive operation for an arbitrary cap yet, so the broker revokes
   --  a grant by refusing its calls. Identifiers are never used again, so
   --  a copy of a revoked grant stays dead.

   Max_Grants : constant := 64;

   type Grant_Record is
      record
         Live      : Boolean := False;
         Id        : Rose.Objects.Capability_Identifier := 0;
         User      : String (1 .. Rights_Policy.Max_User_Length);
         User_Last : Natural := 0;
      end record;

   Grants      : array (1 .. Max_Grants) of Grant_Record;
   Next_Id     : Rose.Objects.Capability_Identifier := 1;

   Context     : Rose.Server.Server_Context;
   Real_Clock  : Clock_Admin_Client.Clock_Admin_Client;

   Rights_Interface_Cap : Rose.Capabilities.Capability :=
                            Rose.Capabilities.Null_Capability;
   Admin_Interface_Cap  : Rose.Capabilities.Capability :=
                            Rose.Capabilities.Null_Capability;

   function Allow
     (Id       : Rose.Objects.Capability_Identifier;
      User     : String;
      Category : String;
      Mode     : Rose.Words.Word)
      return Rose.Words.Word;

   function Find_Grant
     (Id : Rose.Objects.Capability_Identifier)
      return Natural;
   --  The index in Grants of the live grant Id, or 0.

   function Is_Live (Id : Rose.Objects.Capability_Identifier) return Boolean;

   procedure Finish
     (Id    : Rose.Objects.Capability_Identifier;
      User  : Rose.Capabilities.Capability;
      Grant : Rose.Words.Word);

   procedure Get_User_Name
     (User : Rose.Capabilities.Capability;
      Name : out String;
      Last : out Natural);
   --  The name of the user that the User cap is, or Last = 0 if it
   --  isn't a User.

   function Proxy_Monotonic
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64;

   function Proxy_Realtime
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64;

   function Proxy_Realtime_Is_Trusted
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word;

   procedure Proxy_Set_Realtime
     (Id          : Rose.Objects.Capability_Identifier;
      Nanoseconds : Rose.Words.Word_64);

   procedure Request
     (Id        : Rose.Objects.Capability_Identifier;
      User      : Rose.Capabilities.Capability;
      Role_Name : String;
      Elevate   : Rose.Words.Word;
      Grant     : out Rose.Words.Word;
      Granted   : out Rose.Capabilities.Capability);

   procedure Restored (Id : Rose.Objects.Capability_Identifier);

   procedure Revoke (Index : Positive);

   ---------------
   -- Admin_Cap --
   ---------------

   function Admin_Cap return Rose.Capabilities.Capability is
   begin
      return Admin_Interface_Cap;
   end Admin_Cap;

   -----------
   -- Allow --
   -----------

   function Allow
     (Id       : Rose.Objects.Capability_Identifier;
      User     : String;
      Category : String;
      Mode     : Rose.Words.Word)
      return Rose.Words.Word
   is
      pragma Unreferenced (Id);
      Success : Boolean := False;
   begin
      if Mode <= Rights_Policy.Mode'Pos (Rights_Policy.Mode'Last) then

         --  User and Category are the request's buffer, so use them
         --  before this process makes an invocation of its own.

         Rights_Policy.Allow
           (User, Category, Rights_Policy.Mode'Val (Mode), Success);
      end if;

      return (if Success then 1 else 0);
   end Allow;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Clock_Admin : Rose.Interfaces.Clock_Admin.Client.Clock_Admin_Client)
   is
      Self : Rose.Interfaces.Process.Client.Process_Client;
   begin
      Real_Clock := Clock_Admin;

      Rose.Interfaces.Rights.Server.Publish_Interface
        (Server_Context => Context,
         Request        => Request'Access,
         Finish         => Finish'Access);
      Rose.Interfaces.Rights_Admin.Server.Attach_Interface
        (Server_Context => Context,
         Allow          => Allow'Access);
      Clock_Admin_Server.Attach_Interface
        (Server_Context      => Context,
         Monotonic           => Proxy_Monotonic'Access,
         Realtime            => Proxy_Realtime'Access,
         Realtime_Is_Trusted => Proxy_Realtime_Is_Trusted'Access,
         Set_Realtime        => Proxy_Set_Realtime'Access,
         Instanced           => True);
      Rose.Interfaces.Restorable.Server.Attach_Interface
        (Server_Context => Context,
         Restored       => Restored'Access);

      --  The generated server keeps the cap that it publishes to itself,
      --  so ask the process for it, as auth does.

      Rose.Interfaces.Process.Client.Open
        (Self, Rose.Capabilities.Standard.Process_Interface_Cap);
      Rights_Interface_Cap :=
        Rose.Interfaces.Process.Client.Published_Interface (Self);
      Admin_Interface_Cap :=
        Rose.Server.Create_Endpoint
          (Rose.Interfaces.Rights_Admin.Rights_Admin_Interface);
   end Create_Server;

   ----------------
   -- Find_Grant --
   ----------------

   function Find_Grant
     (Id : Rose.Objects.Capability_Identifier)
      return Natural
   is
   begin
      for Index in Grants'Range loop
         if Grants (Index).Live and then Grants (Index).Id = Id then
            return Index;
         end if;
      end loop;
      return 0;
   end Find_Grant;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (Id    : Rose.Objects.Capability_Identifier;
      User  : Rose.Capabilities.Capability;
      Grant : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Name  : String (1 .. Rights_Policy.Max_User_Length);
      Last  : Natural;
      Index : constant Natural :=
                Find_Grant (Rose.Objects.Capability_Identifier (Grant));
   begin
      if Index = 0 then
         return;
      end if;

      Get_User_Name (User, Name, Last);

      --  Only the user that a grant was made to can end it.

      if Last > 0
        and then Name (1 .. Last)
                   = Grants (Index).User (1 .. Grants (Index).User_Last)
      then
         Revoke (Index);
      end if;
   end Finish;

   -------------------
   -- Get_User_Name --
   -------------------

   procedure Get_User_Name
     (User : Rose.Capabilities.Capability;
      Name : out String;
      Last : out Natural)
   is
      Client : Rose.Interfaces.User.Client.User_Client;
   begin
      Last := 0;
      if User = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.Interfaces.User.Client.Open (Client, User);
      Rose.Interfaces.User.Client.Name (Client, Name, Last);
      if Rose.Interfaces.User.Client.Has_Error then
         Last := 0;
      end if;
   end Get_User_Name;

   -------------
   -- Is_Live --
   -------------

   function Is_Live (Id : Rose.Objects.Capability_Identifier) return Boolean
   is (Find_Grant (Id) /= 0);

   ---------------------
   -- Proxy_Monotonic --
   ---------------------

   function Proxy_Monotonic
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64
   is
   begin

      --  A server of the kernel target can't fail a call by raising an
      --  exception, so a revoked grant answers 0 and does nothing.

      if Is_Live (Id) then
         return Clock_Admin_Client.Monotonic (Real_Clock);
      else
         return 0;
      end if;
   end Proxy_Monotonic;

   --------------------
   -- Proxy_Realtime --
   --------------------

   function Proxy_Realtime
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64
   is
   begin
      if Is_Live (Id) then
         return Clock_Admin_Client.Realtime (Real_Clock);
      else
         return 0;
      end if;
   end Proxy_Realtime;

   -------------------------------
   -- Proxy_Realtime_Is_Trusted --
   -------------------------------

   function Proxy_Realtime_Is_Trusted
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word
   is
   begin
      if Is_Live (Id) then
         return Clock_Admin_Client.Realtime_Is_Trusted (Real_Clock);
      else
         return 0;
      end if;
   end Proxy_Realtime_Is_Trusted;

   ------------------------
   -- Proxy_Set_Realtime --
   ------------------------

   procedure Proxy_Set_Realtime
     (Id          : Rose.Objects.Capability_Identifier;
      Nanoseconds : Rose.Words.Word_64)
   is
   begin
      if Is_Live (Id) then
         Clock_Admin_Client.Set_Realtime (Real_Clock, Nanoseconds);
      end if;
   end Proxy_Set_Realtime;

   -------------
   -- Request --
   -------------

   procedure Request
     (Id        : Rose.Objects.Capability_Identifier;
      User      : Rose.Capabilities.Capability;
      Role_Name : String;
      Elevate   : Rose.Words.Word;
      Grant     : out Rose.Words.Word;
      Granted   : out Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);

      --  Role_Name is the request's buffer, so copy it before this
      --  process makes an invocation of its own.

      Role      : constant String (1 .. Role_Name'Length) := Role_Name;
      Name      : String (1 .. Rights_Policy.Max_User_Length);
      Last      : Natural;
      Asked     : constant Boolean := Elevate /= 0;
      Known     : constant Boolean := Role = Clock_Role;
      Allowed   : Boolean := False;
      Slot      : Natural := 0;

      procedure Report (Outcome : String);
      --  Write a line for the request on standard output (ADR 0021,
      --  section 6).

      ------------
      -- Report --
      ------------

      procedure Report (Outcome : String) is
      begin
         Ada.Text_IO.Put ("rights: ");
         if Last > 0 then
            Ada.Text_IO.Put (Name (1 .. Last));
         else
            Ada.Text_IO.Put ("(unknown user)");
         end if;
         Ada.Text_IO.Put (" asked for " & Role);
         if Known then
            Ada.Text_IO.Put (" (" & Clock_Category & ")");
         end if;
         if Asked then
            Ada.Text_IO.Put (" with sudo");
         end if;
         Ada.Text_IO.Put_Line (": " & Outcome);
         Ada.Text_IO.Flush;
      end Report;

   begin
      Grant := 0;
      Granted := Rose.Capabilities.Null_Capability;

      Get_User_Name (User, Name, Last);

      if Known and then Last > 0 then
         case Rights_Policy.Lookup (Name (1 .. Last), Clock_Category) is
            when Rights_Policy.None     => Allowed := False;
            when Rights_Policy.Elevate  => Allowed := Asked;
            when Rights_Policy.Granted  => Allowed := True;
         end case;
      end if;

      if not Allowed then
         Report ("refused");
         return;
      end if;

      for Index in Grants'Range loop
         if not Grants (Index).Live then
            Slot := Index;
            exit;
         end if;
      end loop;

      if Slot = 0 then
         Report ("refused, too many grants");
         return;
      end if;

      Grants (Slot) :=
        (Live      => True,
         Id        => Next_Id,
         User      => (others => ' '),
         User_Last => Last);
      Grants (Slot).User (1 .. Last) := Name (1 .. Last);
      Grant := Rose.Words.Word (Next_Id);
      Granted :=
        Rose.Server.Create_Endpoint
          (Rose.Interfaces.Clock_Admin.Clock_Admin_Interface, Next_Id);
      Next_Id := Next_Id + 1;

      Report ("granted");
   end Request;

   --------------
   -- Restored --
   --------------

   procedure Restored (Id : Rose.Objects.Capability_Identifier) is
      pragma Unreferenced (Id);
   begin

      --  Elevation comes from an authentication, and a restore ends
      --  authentication (ADR 0018), so it ends every grant.

      for Index in Grants'Range loop
         if Grants (Index).Live then
            Revoke (Index);
         end if;
      end loop;
   end Restored;

   ------------
   -- Revoke --
   ------------

   procedure Revoke (Index : Positive) is
   begin
      Grants (Index).Live := False;

      --  Free the grant's instance caps. The proxy refuses the grant's
      --  calls whether or not they are still there.

      Clock_Admin_Server.Remove_Instance (Grants (Index).Id);
   end Revoke;

   ----------------
   -- Rights_Cap --
   ----------------

   function Rights_Cap return Rose.Capabilities.Capability is
   begin
      return Rights_Interface_Cap;
   end Rights_Cap;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Rights_Server;
