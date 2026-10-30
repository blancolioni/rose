with Ada.Text_IO;

with Tests.Registry;
with Tests.Suite;

procedure Rose.Core.Run_Self_Tests
  (Find_Command : in out Rose.Interfaces.Commands.Commands_Object'Class)
is
   Self_Tests_Failed : exception;
   --  Raised when a test fails, so that the command fails, and its
   --  caller can tell (#460).

   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;

   function Image (X : Natural) return String;
   --  X, with no leading space.

   -----------
   -- Image --
   -----------

   function Image (X : Natural) return String is
      Text : constant String := Natural'Image (X);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

begin
   Tests.Registry.Set_Commands (Find_Command);

   Ada.Text_IO.Put_Line ("Starting tests");

   for Test of Tests.Suite.Test_Suite loop
      declare
         Success : constant Boolean := Test.all;
      begin
         Tests_Run := Tests_Run + 1;
         if Success then
            Ada.Text_IO.Put_Line ("PASS");
            Tests_Passed := Tests_Passed + 1;
         else
            Ada.Text_IO.Put_Line ("FAIL");
         end if;
      end;
   end loop;

   Ada.Text_IO.Put_Line
     ("Test result: tests " & Image (Tests_Run)
      & "; passed " & Image (Tests_Passed)
      & "; score " & Image (Tests_Passed * 100 / Tests_Run) & "%");

   if Tests_Passed < Tests_Run then
      raise Self_Tests_Failed with
        Image (Tests_Run - Tests_Passed) & " of " & Image (Tests_Run)
        & " self tests failed";
   end if;
end Rose.Core.Run_Self_Tests;
