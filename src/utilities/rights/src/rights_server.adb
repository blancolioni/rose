with Ada.Text_IO;

with Rose.Server;
with Rose.Words;

with Rose.Interfaces.Clock_Admin.Client;
with Rose.Interfaces.Clock_Admin.Server;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.Rights.Server;
with Rose.Interfaces.Rights_Admin.Server;
with Rose.Interfaces.User;

with Rights_Policy;

package body Rights_Server is

   use type Rose.Words.Word;

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

   --  The broker serves Rights, and takes the restore notice, through one
   --  object, and Rights_Admin through another.

   type Broker_Object is
     new Rose.Interfaces.Rights.Server.Rights_Interface
     and Rose.Interfaces.Restorable.Server.Restorable_Interface
     with null record;

   overriding function Rights_Cap
     (Item : in out Broker_Object)
      return Rose.Capabilities.Capability;

   overriding function Restorable_Cap
     (Item : in out Broker_Object)
      return Rose.Capabilities.Capability;

   overriding procedure Request
     (Item      : in out Broker_Object;
      User      : in out Rose.Interfaces.User.User_Object'Class;
      Role_Name : String;
      Elevate   : Rose.Words.Word;
      Grant     : out Rose.Words.Word;
      Granted   : out Rose.Capabilities.Capability);

   overriding procedure Finish
     (Item  : in out Broker_Object;
      User  : in out Rose.Interfaces.User.User_Object'Class;
      Grant : Rose.Words.Word);

   overriding procedure Restored (Item : in out Broker_Object);

   type Admin_Object is
     new Rose.Interfaces.Rights_Admin.Server.Rights_Admin_Interface
     with null record;

   overriding function Rights_Admin_Cap
     (Item : in out Admin_Object)
      return Rose.Capabilities.Capability;

   overriding function Allow
     (Item     : in out Admin_Object;
      User     : String;
      Category : String;
      Mode     : Rose.Words.Word)
      return Rose.Words.Word;

   --  A grant is a proxy for the role's interface, which the broker
   --  serves and forwards to the cap that it holds. A server can't take
   --  back the caps of one object yet (#769), so the broker revokes a
   --  grant by failing its calls. Each grant is a new object, which is
   --  never attached again, so a copy of a revoked grant stays dead.

   type Grant_Object is
     new Clock_Admin_Server.Clock_Admin_Interface with
      record
         Live      : Boolean := False;
         Number    : Rose.Words.Word := 0;
         User      : String (1 .. Rights_Policy.Max_User_Length);
         User_Last : Natural := 0;
      end record;

   overriding function Clock_Cap
     (Item : in out Grant_Object)
      return Rose.Capabilities.Capability;

   overriding function Clock_Admin_Cap
     (Item : in out Grant_Object)
      return Rose.Capabilities.Capability;

   overriding function Monotonic
     (Item : in out Grant_Object)
      return Rose.Words.Word_64;

   overriding function Realtime
     (Item : in out Grant_Object)
      return Rose.Words.Word_64;

   overriding function Realtime_Is_Trusted
     (Item : in out Grant_Object)
      return Boolean;

   overriding procedure Set_Realtime
     (Item        : in out Grant_Object;
      Nanoseconds : Rose.Words.Word_64);

   procedure Check (Item : Grant_Object'Class);
   --  Fail the call if Item has been revoked.

   type Grant_Access is access all Grant_Object;

   Max_Grants : constant := 64;
   --  The most grants that are live at once.

   Grants      : array (1 .. Max_Grants) of Grant_Access;
   Next_Number : Rose.Words.Word := 1;
   --  The number of the next grant, which the requester is given, to
   --  finish it with.

   Context    : Rose.Server.Server_Context;
   The_Broker : aliased Broker_Object;
   The_Admin  : aliased Admin_Object;
   Real_Clock : Rose.Interfaces.Clock_Admin.Client.Clock_Admin_Client;

   function Find_Grant (Number : Rose.Words.Word) return Natural;
   --  The index in Grants of the live grant Number, or 0.

   procedure Get_User_Name
     (User : in out Rose.Interfaces.User.User_Object'Class;
      Name : out String;
      Last : out Natural);
   --  The name of User, or Last = 0 if it can't be asked.

   procedure Revoke (Index : Positive);

   ---------------
   -- Admin_Cap --
   ---------------

   function Admin_Cap return Rose.Capabilities.Capability is
   begin
      return Rose.Interfaces.Rights_Admin.Server.Cap_Of (The_Admin);
   end Admin_Cap;

   -----------
   -- Allow --
   -----------

   overriding function Allow
     (Item     : in out Admin_Object;
      User     : String;
      Category : String;
      Mode     : Rose.Words.Word)
      return Rose.Words.Word
   is
      pragma Unreferenced (Item);

      --  User and Category are the request's buffer, so copy them before
      --  this process makes an invocation of its own, which writing the
      --  audit line does.

      Who           : constant String (1 .. User'Length) := User;
      Category_Name : constant String (1 .. Category'Length) := Category;
      Success       : Boolean := False;
   begin
      if Mode <= Rights_Policy.Mode'Pos (Rights_Policy.Mode'Last) then
         Rights_Policy.Allow
           (Who, Category_Name, Rights_Policy.Mode'Val (Mode), Success);
      end if;

      --  A change of policy is audited as a request is (ADR 0021,
      --  section 6).

      Ada.Text_IO.Put ("rights: allow " & Who & " " & Category_Name & ": ");
      if not Success then
         Ada.Text_IO.Put_Line ("refused");
      else
         Ada.Text_IO.Put_Line
           (case Rights_Policy.Mode'Val (Mode) is
               when Rights_Policy.None    => "none",
               when Rights_Policy.Elevate => "elevate",
               when Rights_Policy.Granted => "granted");
      end if;
      Ada.Text_IO.Flush;

      return (if Success then 1 else 0);
   end Allow;

   -----------
   -- Check --
   -----------

   procedure Check (Item : Grant_Object'Class) is
   begin
      if not Item.Live then
         raise Constraint_Error with "rights: the grant was revoked";
      end if;
   end Check;

   ---------------------
   -- Clock_Admin_Cap --
   ---------------------

   overriding function Clock_Admin_Cap
     (Item : in out Grant_Object)
      return Rose.Capabilities.Capability
   is (Clock_Admin_Server.Cap_Of (Item));

   ---------------
   -- Clock_Cap --
   ---------------

   overriding function Clock_Cap
     (Item : in out Grant_Object)
      return Rose.Capabilities.Capability
   is (Clock_Admin_Server.Cap_Of
         (Item, Rose.Interfaces.Clock_Admin.Clock_Interface));

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server (Clock_Admin : Rose.Capabilities.Capability) is
   begin
      Rose.Interfaces.Clock_Admin.Client.Open (Real_Clock, Clock_Admin);
      Rose.Interfaces.Rights.Server.Publish (Context, The_Broker'Access);
      Rose.Interfaces.Rights_Admin.Server.Attach (Context, The_Admin'Access);
      Rose.Interfaces.Restorable.Server.Attach (Context, The_Broker'Access);
   end Create_Server;

   ----------------
   -- Find_Grant --
   ----------------

   function Find_Grant (Number : Rose.Words.Word) return Natural is
   begin
      for Index in Grants'Range loop
         if Grants (Index) /= null
           and then Grants (Index).Live
           and then Grants (Index).Number = Number
         then
            return Index;
         end if;
      end loop;
      return 0;
   end Find_Grant;

   ------------
   -- Finish --
   ------------

   overriding procedure Finish
     (Item  : in out Broker_Object;
      User  : in out Rose.Interfaces.User.User_Object'Class;
      Grant : Rose.Words.Word)
   is
      pragma Unreferenced (Item);
      Name  : String (1 .. Rights_Policy.Max_User_Length);
      Last  : Natural;
      Index : constant Natural := Find_Grant (Grant);
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
     (User : in out Rose.Interfaces.User.User_Object'Class;
      Name : out String;
      Last : out Natural)
   is
   begin
      Last := 0;
      declare
         User_Name : constant String := User.Name;
         Length    : constant Natural :=
                       Natural'Min (User_Name'Length, Name'Length);
      begin
         Name (Name'First .. Name'First + Length - 1) :=
           User_Name (User_Name'First .. User_Name'First + Length - 1);
         Last := Name'First + Length - 1;
      end;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>

         --  Not a User: a null cap, or one that fails.

         Last := 0;
   end Get_User_Name;

   ---------------
   -- Monotonic --
   ---------------

   overriding function Monotonic
     (Item : in out Grant_Object)
      return Rose.Words.Word_64
   is
   begin
      Check (Item);
      return Real_Clock.Monotonic;
   end Monotonic;

   --------------
   -- Realtime --
   --------------

   overriding function Realtime
     (Item : in out Grant_Object)
      return Rose.Words.Word_64
   is
   begin
      Check (Item);
      return Real_Clock.Realtime;
   end Realtime;

   -------------------------
   -- Realtime_Is_Trusted --
   -------------------------

   overriding function Realtime_Is_Trusted
     (Item : in out Grant_Object)
      return Boolean
   is
   begin
      Check (Item);
      return Real_Clock.Realtime_Is_Trusted;
   end Realtime_Is_Trusted;

   -------------
   -- Request --
   -------------

   overriding procedure Request
     (Item      : in out Broker_Object;
      User      : in out Rose.Interfaces.User.User_Object'Class;
      Role_Name : String;
      Elevate   : Rose.Words.Word;
      Grant     : out Rose.Words.Word;
      Granted   : out Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Item);

      --  Role_Name is the request's buffer, so copy it before this
      --  process makes an invocation of its own.

      Role    : constant String (1 .. Role_Name'Length) := Role_Name;
      Name    : String (1 .. Rights_Policy.Max_User_Length);
      Last    : Natural;
      Asked   : constant Boolean := Elevate /= 0;
      Known   : constant Boolean := Role = Clock_Role;
      Allowed : Boolean := False;
      Slot    : Natural := 0;

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
         if Grants (Index) = null or else not Grants (Index).Live then
            Slot := Index;
            exit;
         end if;
      end loop;

      if Slot = 0 then
         Report ("refused, too many grants");
         return;
      end if;

      --  A new object for every grant: the revoked one in this slot, if
      --  there was one, stays attached, and keeps failing its calls.

      Grants (Slot) :=
        new Grant_Object'
          (Live      => True,
           Number    => Next_Number,
           User      => (others => ' '),
           User_Last => Last);
      Grants (Slot).User (1 .. Last) := Name (1 .. Last);
      Clock_Admin_Server.Attach (Context, Grants (Slot));

      Grant := Next_Number;
      Granted := Clock_Admin_Server.Cap_Of (Grants (Slot).all);
      Next_Number := Next_Number + 1;

      Report ("granted");
   end Request;

   --------------------
   -- Restorable_Cap --
   --------------------

   overriding function Restorable_Cap
     (Item : in out Broker_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Restorable.Server.Cap_Of (Item));

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Broker_Object) is
      pragma Unreferenced (Item);
   begin

      --  Elevation comes from an authentication, and a restore ends
      --  authentication (ADR 0018), so it ends every grant.

      for Index in Grants'Range loop
         if Grants (Index) /= null and then Grants (Index).Live then
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
   end Revoke;

   ----------------------
   -- Rights_Admin_Cap --
   ----------------------

   overriding function Rights_Admin_Cap
     (Item : in out Admin_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Rights_Admin.Server.Cap_Of (Item));

   ----------------
   -- Rights_Cap --
   ----------------

   function Rights_Cap return Rose.Capabilities.Capability is
   begin
      return Rose.Interfaces.Rights.Server.Cap_Of (The_Broker);
   end Rights_Cap;

   ----------------
   -- Rights_Cap --
   ----------------

   overriding function Rights_Cap
     (Item : in out Broker_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Rights.Server.Cap_Of (Item));

   ------------------
   -- Set_Realtime --
   ------------------

   overriding procedure Set_Realtime
     (Item        : in out Grant_Object;
      Nanoseconds : Rose.Words.Word_64)
   is
   begin
      Check (Item);
      Real_Clock.Set_Realtime (Nanoseconds);
   end Set_Realtime;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Rights_Server;
