with Ada.Text_IO;

package body Tests.Smoke is

   ----------------
   -- Smoke_Test --
   ----------------

   function Smoke_Test return Boolean is
   begin
      Ada.Text_IO.Put ("Smoke test");
      return True;
   end Smoke_Test;

end Tests.Smoke;
