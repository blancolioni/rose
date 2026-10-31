with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Standard_Streams;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Clock.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Rights.Client;
with Rose.Interfaces.Rights_Admin.Client;
with Rose.Interfaces.User.Client;

with Tests.Registry;
with Tests.Users;

package body Tests.Rights is

   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word;

   Role : constant String := "Clock_Admin_Service";

   None    : constant Rose.Words.Word := 0;
   Elevate : constant Rose.Words.Word := 1;
   Granted : constant Rose.Words.Word := 2;
   --  The modes of Rights_Admin.Allow.

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

   -----------------
   -- Rights_Test --
   -----------------

   function Rights_Test return Boolean is
      Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
                   Tests.Registry.Launcher ("rights");
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Endpoint : Rose.Capabilities.Capability;
      Params   : aliased Rose.Invocation.Invocation_Record;

      Rights   : Rose.Interfaces.Rights.Client.Rights_Client;
      Admin    : Rose.Interfaces.Rights_Admin.Client.Rights_Admin_Client;
      Alice    : Rose.Interfaces.User.Client.User_Client;
      Bob      : Rose.Interfaces.User.Client.User_Client;

      Grant    : Rose.Words.Word;
      Cap      : Rose.Capabilities.Capability;

      function Ask
        (User : in out Rose.Interfaces.User.User_Object'Class;
         Sudo : Rose.Words.Word;
         Name : String := Role)
         return Boolean;
      --  True if the broker gave User a cap for the role Name, asked for
      --  with sudo if Sudo is 1. The cap and the grant are Cap and Grant.

      ---------
      -- Ask --
      ---------

      function Ask
        (User : in out Rose.Interfaces.User.User_Object'Class;
         Sudo : Rose.Words.Word;
         Name : String := Role)
         return Boolean
      is
      begin
         Cap := Rose.Capabilities.Null_Capability;
         Rights.Request (User, Name, Sudo, Grant, Cap);
         return Cap /= Rose.Capabilities.Null_Capability;
      end Ask;

   begin
      Ada.Text_IO.Put ("Rights test");

      if Tests.Registry.Clock = Rose.Capabilities.Null_Capability then
         return Fail ("no clock");
      end if;

      --  The broker's roles' caps, in the order of its command
      --  declaration (src/utilities/rights/rose-core-rights.idl). The
      --  clock's Clock interface stands for the admin one: it reads, and
      --  can't set the time.

      Endpoint := Launcher.Launch_Command (No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Fail ("cannot launch the broker");
      end if;

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Tests.Registry.Create_Endpoint);
      Rose.System_Calls.Send_Cap
        (Params, Rose.Standard_Streams.Standard_Output);
      Rose.System_Calls.Send_Cap (Params, Tests.Registry.Clock);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= 0
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
        or else Params.Caps (0) = Rose.Capabilities.Null_Capability
        or else Params.Caps (1) = Rose.Capabilities.Null_Capability
      then
         return Fail ("the broker did not start");
      end if;

      Rights.Open (Params.Caps (0));
      Admin.Open (Params.Caps (1));

      --  Each user has a home directory of their own. This is where the
      --  test would stall when mem's page table was too small (#591).

      declare
         Alice_Home : constant Rose.Capabilities.Capability :=
                        Tests.Registry.Launch
                          ("directory",
                           (Tests.Registry.Launcher_Cap ("file"),
                            Tests.Registry.Launcher_Cap ("directory")));
         Bob_Home   : constant Rose.Capabilities.Capability :=
                        Tests.Registry.Launch
                          ("directory",
                           (Tests.Registry.Launcher_Cap ("file"),
                            Tests.Registry.Launcher_Cap ("directory")));
      begin
         Alice.Open (Tests.Users.Create_User ("alice", Alice_Home));
         Bob.Open (Tests.Users.Create_User ("bob", Bob_Home));
      end;

      --  With no policy, nobody gets anything.

      if Ask (Alice, 1) then
         return Fail ("a grant with no policy");
      end if;

      --  Alice may elevate to the time category, and so can't without it,
      --  and Bob can't at all. A role that doesn't exist is refused.

      if Admin.Allow ("alice", "time", 3) /= 0 then
         return Fail ("a mode that doesn't exist");
      elsif Admin.Allow ("alice", "time", Elevate) /= 1 then
         return Fail ("cannot set a policy");
      end if;

      if Ask (Alice, 0) then
         return Fail ("a grant without sudo");
      elsif Ask (Bob, 1) then
         return Fail ("a grant to another user");
      elsif Ask (Alice, 1, "No_Such_Role") then
         return Fail ("a grant of a role that doesn't exist");
      elsif not Ask (Alice, 1) or else Grant = 0 then
         return Fail ("no grant under sudo");
      end if;

      --  The grant works: it is the clock, through the broker.

      declare
         Clock        : Rose.Interfaces.Clock.Client.Clock_Client;
         Before       : Rose.Words.Word_64;
         After        : Rose.Words.Word_64;
         Alices_Grant : constant Rose.Words.Word := Grant;
         use type Rose.Words.Word_64;
      begin
         Clock.Open (Cap);
         Before := Clock.Monotonic;
         if Before = 0 then
            return Fail ("the grant does not reach the clock");
         end if;

         --  Bob can't end Alice's grant.

         Rights.Finish (Bob, Alices_Grant);
         After := Clock.Monotonic;
         if After < Before then
            return Fail ("another user ended the grant");
         end if;

         --  Alice can, and then the grant is dead, and so is any copy.

         Rights.Finish (Alice, Alices_Grant);
         After := Clock.Monotonic;
         if After /= 0 then
            return Fail ("the grant works after it was finished");
         end if;
      end;

      --  The most specific policy wins: Bob may have everything in an
      --  ordinary launch, except the time, which he may not have at all.

      if Admin.Allow ("bob", "*", Granted) /= 1 then
         return Fail ("cannot set a policy for every category");
      elsif not Ask (Bob, 0) then
         return Fail ("no grant for a policy that covers everything");
      end if;
      Rights.Finish (Bob, Grant);

      if Admin.Allow ("bob", "time", None) /= 1 then
         return Fail ("cannot set a policy against a category");
      elsif Ask (Bob, 1) then
         return Fail ("a grant against the most specific policy");
      end if;

      return True;
   end Rights_Test;

end Tests.Rights;
