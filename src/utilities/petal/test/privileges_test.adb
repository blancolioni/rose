with Ada.Text_IO;

with Petal.Privileges;

--  Host test of Petal.Privileges (make test in src/utilities/petal).

procedure Privileges_Test is

   use Petal.Privileges;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Message : String);

   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Message);
      end if;
   end Check;

begin
   Check (Covers ("time", "time"), "a category covers itself");
   Check (Covers ("network", "network.dns"), "a category covers below it");
   Check (not Covers ("network", "networking"),
          "a category doesn't cover another with its name as a prefix");
   Check (not Covers ("network.dns", "network"),
          "a category doesn't cover the one above it");
   Check (not Covers ("time", "network"), "unrelated categories");
   Check (Covers ("*", "anything.at.all"), "* covers every category");

   Check (Count ("") = 0, "no names");
   Check (Count ("time") = 1, "one name");
   Check (Count ("network,time") = 2, "two names");
   Check (Item ("network,time", 1) = "network", "first name");
   Check (Item ("network,time", 2) = "time", "second name");
   Check (Item ("network", 2) = "", "a name that isn't there");

   Check (Covered ("network,time", "time"), "the last name covers");
   Check (Covered ("network,time", "network.dns"), "the first name covers");
   Check (not Covered ("network,time", "accounts"), "no name covers");
   Check (not Covered ("", "time"), "no names cover nothing");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("privileges_test: ok");
   else
      Ada.Text_IO.Put_Line ("privileges_test:" & Failures'Image & " failed");
      raise Program_Error;
   end if;
end Privileges_Test;
