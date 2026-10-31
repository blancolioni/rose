with Ada.Text_IO;

with Rights_Policy;

--  Host test of Rights_Policy (make test in src/utilities/rights).

procedure Rights_Policy_Test is

   use Rights_Policy;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Message : String);
   procedure Allowed (User, Category : String; Value : Mode);

   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Message);
      end if;
   end Check;

   procedure Allowed (User, Category : String; Value : Mode) is
      Success : Boolean;
   begin
      Allow (User, Category, Value, Success);
      Check (Success, "allow " & User & " " & Category);
   end Allowed;

begin
   --  What a policy covers.

   Check (Covers ("time", "time"), "the same");
   Check (Covers ("hardware", "hardware.ports"), "below");
   Check (Covers ("hardware", "hardware.ports.io"), "further below");
   Check (Covers ("*", "time"), "a star covers everything");
   Check (not Covers ("", "time"), "empty covers nothing");
   Check (not Covers ("hard", "hardware"), "a prefix of a part is not below");
   Check (not Covers ("hardware.ports", "hardware"), "above is not covered");
   Check (not Covers ("time", "accounts"), "another category");

   --  No policy is none.

   Check (Lookup ("alice", "time") = None, "default");

   --  A policy is for a user, and a category.

   Allowed ("alice", "time", Elevate);
   Check (Lookup ("alice", "time") = Elevate, "alice time");
   Check (Lookup ("alice", "accounts") = None, "alice accounts");
   Check (Lookup ("bob", "time") = None, "bob time");

   --  A later policy replaces an earlier one.

   Allowed ("alice", "time", Granted);
   Check (Lookup ("alice", "time") = Granted, "replaced");

   --  The most specific policy wins, whatever order they came in.

   Allowed ("carol", "*", Elevate);
   Allowed ("carol", "hardware", Granted);
   Allowed ("carol", "hardware.ports", None);
   Check (Lookup ("carol", "time") = Elevate, "carol everything");
   Check (Lookup ("carol", "hardware") = Granted, "carol hardware");
   Check (Lookup ("carol", "hardware.disk") = Granted, "carol below");
   Check (Lookup ("carol", "hardware.ports") = None, "carol most specific");
   Check (Lookup ("carol", "hardware.ports.io") = None, "carol further");

   --  A user's name is the whole name.

   Allowed ("al", "time", Elevate);
   Check (Lookup ("alice", "time") = Granted, "al is not alice");
   Check (Lookup ("al", "time") = Elevate, "al");

   --  Names that are too long, and no name, are refused.

   declare
      Success : Boolean;
      Long    : constant String (1 .. Max_User_Length + 1) := (others => 'x');
   begin
      Allow (Long, "time", Elevate, Success);
      Check (not Success, "a user name that is too long");
      Allow ("", "time", Elevate, Success);
      Check (not Success, "no user name");
      Allow ("dave", (1 .. Max_Category_Length + 1 => 'x'), Elevate, Success);
      Check (not Success, "a category that is too long");
   end;

   --  The table fills up, and then replacing still works.

   declare
      Success : Boolean := True;
      Number  : Natural := 0;
   begin
      while Success loop
         Number := Number + 1;
         Allow ("user" & Natural'Image (Number), "time", Elevate, Success);
         exit when Number > Max_Policies + 1;
      end loop;
      Check (not Success, "a full table refuses another");
      Allow ("alice", "time", None, Success);
      Check (Success, "a full table can still replace");
      Check (Lookup ("alice", "time") = None, "replaced in a full table");
   end;

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("rights policy: ok");
   else
      Ada.Text_IO.Put_Line ("rights policy:" & Natural'Image (Failures)
                            & " failed");
      raise Program_Error;
   end if;
end Rights_Policy_Test;
