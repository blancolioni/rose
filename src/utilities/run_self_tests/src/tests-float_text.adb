with Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Text_IO;

package body Tests.Float_Text is

   Passed : Boolean := True;

   procedure Check
     (Name      : String;
      Got, Want : String);
   --  Fail, writing Name and both texts, unless Got is Want.

   procedure Fail (Name : String);
   --  Write Name as a failed check, and fail the test.

   procedure Not_Raised
     (Name  : String;
      Value : Float);
   --  Fail: Value is the result of a call that should have raised.

   -----------
   -- Check --
   -----------

   procedure Check
     (Name      : String;
      Got, Want : String)
   is
   begin
      if Got /= Want then
         Fail (Name & " [" & Got & "] not [" & Want & "]");
      end if;
   end Check;

   ----------
   -- Fail --
   ----------

   procedure Fail (Name : String) is
   begin
      Ada.Text_IO.Put (": " & Name);
      Passed := False;
   end Fail;

   ---------------------
   -- Float_Text_Test --
   ---------------------

   function Float_Text_Test return Boolean is
   begin
      Ada.Text_IO.Put ("Float text test");
      Passed := True;

      --  'Image has Digits - 1 figures after the point, and an exponent.

      Check ("Float'Image (1.5)", Float'Image (1.5), " 1.50000E+00");
      Check ("Float'Image (-0.25)", Float'Image (-0.25), "-2.50000E-01");
      Check ("Long_Float'Image (Pi)",
             Long_Float'Image (3.14159_26535_89793),
             " 3.14159265358979E+00");
      Check ("Long_Long_Float'Image (1.0E100)",
             Long_Long_Float'Image (1.0E100),
             " 1.00000000000000000E+100");

      --  'Value gives the same value as the literal.

      if Float'Value ("0.1") /= 0.1 then
         Fail ("Float'Value (""0.1"")");
      end if;
      if Long_Float'Value ("-2.5E-3") /= -2.5E-3 then
         Fail ("Long_Float'Value (""-2.5E-3"")");
      end if;
      if Long_Long_Float'Value (" 1.0E100 ") /= 1.0E100 then
         Fail ("Long_Long_Float'Value ("" 1.0E100 "")");
      end if;
      if Long_Float'Value (Long_Float'Image (1.0 / 3.0))
        /= Long_Float'Value ("3.33333333333333E-01")
      then
         Fail ("Long_Float'Value of its Image");
      end if;

      begin
         Not_Raised ("Float'Value (""one"") did not raise Constraint_Error",
                     Float'Value ("one"));
      exception
         when Constraint_Error =>
            null;
      end;

      --  Fixed point, 32-bit and 64-bit, and decimal.

      declare
         type Small_Fixed is delta 0.01 range -100.0 .. 100.0;
         type Large_Fixed is delta 0.001 range -1.0E9 .. 1.0E9;
         type Money is delta 0.01 digits 10;
      begin
         Check ("Small_Fixed'Image (-1.25)",
                Small_Fixed'Image (-1.25), "-1.25");
         Check ("Large_Fixed'Image (123456789.125)",
                Large_Fixed'Image (123_456_789.125), " 123456789.125");
         Check ("Money'Image (3.5)", Money'Image (3.5), " 3.50");
         if Small_Fixed'Value ("12.5") /= 12.5 then
            Fail ("Small_Fixed'Value (""12.5"")");
         end if;
         if Money'Value ("-42.07") /= -42.07 then
            Fail ("Money'Value (""-42.07"")");
         end if;
      end;

      --  Float_Text_IO on strings.

      declare
         Text : String (1 .. 10);
      begin
         Ada.Float_Text_IO.Put (Text, 3.25, Aft => 2, Exp => 0);
         Check ("Float_Text_IO.Put (3.25, Aft => 2, Exp => 0)",
                Text, "      3.25");
         Ada.Long_Float_Text_IO.Put (Text, -1.5E-7, Aft => 3, Exp => 2);
         Check ("Long_Float_Text_IO.Put (-1.5E-7, Aft => 3, Exp => 2)",
                Text, " -1.500E-7");
      end;

      declare
         Item : Float;
         Last : Positive;
      begin
         Ada.Float_Text_IO.Get ("2.5E2 rest", Item, Last);
         if Item /= 250.0 or else Last /= 5 then
            Fail ("Float_Text_IO.Get (""2.5E2 rest"")");
         end if;
      end;

      return Passed;
   end Float_Text_Test;

   ----------------
   -- Not_Raised --
   ----------------

   procedure Not_Raised
     (Name  : String;
      Value : Float)
   is
      pragma Unreferenced (Value);
   begin
      Fail (Name);
   end Not_Raised;

end Tests.Float_Text;
