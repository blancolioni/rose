with Ada.Text_IO;

package body Tests.Stack is

   Test_Size : constant := 1000;
   type Test_Array is array (1 .. Test_Size) of Positive;

   ----------------
   -- Stack_Test --
   ----------------

   function Stack_Test return Boolean is
      Numbers : Test_Array;
      Sum     : Natural := 0;
   begin
      Ada.Text_IO.Put ("Stack test");
      for I in Numbers'Range loop
         Numbers (I) := I;
      end loop;

      for I in 1 .. Numbers'Last / 2 loop
         declare
            T : constant Positive := Numbers (I);
         begin
            Numbers (I) := Numbers (Numbers'Last + 1 - I);
            Numbers (Numbers'Last + 1 - I) := T;
         end;
      end loop;

      for I in Numbers'Range loop
         Sum := Sum + Numbers (I);
      end loop;

      return Sum = Test_Size * (Test_Size + 1) / 2;
   end Stack_Test;

end Tests.Stack;
