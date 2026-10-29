with Ada.Exceptions;
with Ada.Text_IO;

package body Tests.Exceptions is

   Test_Error : exception;

   Handled : Natural := 0;
   --  The number of times Reraise handled Test_Error.

   procedure Level_1;
   procedure Level_2;
   procedure Level_3;
   --  Level_1 calls Level_2, which calls Level_3, which raises Test_Error.

   function Element (Index : Positive) return Character;
   --  A character of a short string: Constraint_Error if Index is past its
   --  end.

   procedure Reraise;
   --  Handle Test_Error, and raise it again.

   -------------
   -- Element --
   -------------

   function Element (Index : Positive) return Character is
      pragma Unsuppress (All_Checks);
      --  The tests program is compiled with checks suppressed (-gnatp),
      --  and Index_Check alone does not bring this check back.
      Text : constant String := "rose";
   begin
      return Text (Index);
   end Element;

   --------------------
   -- Exception_Test --
   --------------------

   function Exception_Test return Boolean is
      use Ada.Exceptions;
   begin
      Ada.Text_IO.Put ("Exception test");

      --  Raised three calls down, with a message.

      begin
         Level_1;
         return False;
      exception
         when E : Test_Error =>
            if Exception_Name (E) /= "TESTS.EXCEPTIONS.TEST_ERROR"
              or else Exception_Message (E) /= "depth reached"
            then
               return False;
            end if;
      end;

      --  A failed index check.

      declare
         Ch : Character := ' ';
      begin
         Ch := Element (Index => 5);
         return Ch = ' ';
      exception
         when Constraint_Error =>
            null;
      end;

      --  Handled, raised again, and handled by the caller.

      begin
         Reraise;
         return False;
      exception
         when E : others =>
            return Exception_Identity (E) = Test_Error'Identity
              and then Handled = 1;
      end;
   end Exception_Test;

   -------------
   -- Level_1 --
   -------------

   procedure Level_1 is
   begin
      Level_2;
   end Level_1;

   -------------
   -- Level_2 --
   -------------

   procedure Level_2 is
   begin
      Level_3;
   end Level_2;

   -------------
   -- Level_3 --
   -------------

   procedure Level_3 is
   begin
      raise Test_Error with "depth reached";
   end Level_3;

   -------------
   -- Reraise --
   -------------

   procedure Reraise is
   begin
      Level_2;
   exception
      when Test_Error =>
         Handled := Handled + 1;
         raise;
   end Reraise;

end Tests.Exceptions;
