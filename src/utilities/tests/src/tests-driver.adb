with Ada.Text_IO;

with Tests.Suite;

procedure Tests.Driver is
   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;

   procedure Put (X : Natural);

   ---------
   -- Put --
   ---------

   procedure Put (X : Natural) is
   begin
      if X = 0 then
         Ada.Text_IO.Put ("0");
      else
         declare
            It : Natural := X;
            S  : String (1 .. 10);
            First : Natural := S'Last;
         begin
            while It /= 0 loop
               S (First) := Character'Val (It mod 10 + 48);
               First := First - 1;
               It := It / 10;
            end loop;
            Ada.Text_IO.Put (S (First + 1 .. S'Last));
         end;
      end if;
   end Put;

begin
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

   declare
      Pass_Rate : constant Natural := Tests_Passed * 100 / Tests_Run;
   begin
      Ada.Text_IO.Put ("Test result: tests ");
      Put (Tests_Run);
      Ada.Text_IO.Put ("; passed ");
      Put (Tests_Passed);
      Ada.Text_IO.Put ("; score ");
      Put (Pass_Rate);
      Ada.Text_IO.Put_Line ("%");
   end;

end Tests.Driver;
