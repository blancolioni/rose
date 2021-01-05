package body Petal.Values is

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Value : Petal_Value) return Boolean is
      S : constant String := To_String (Value);
   begin
      return S /= "" and then S /= "false";
   end To_Boolean;

   -------------------
   -- To_Capability --
   -------------------

   function To_Capability
     (Value : Petal_Value) return Rose.Capabilities.Capability
   is
   begin
      return Rose.Capabilities.Capability (To_Integer (Value));
   end To_Capability;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : Petal_Value) return Integer is
      S     : constant String := To_String (Value);
      First : Boolean := True;
      Neg   : Boolean := False;
      Acc   : Integer := 0;
   begin
      for Ch of S loop
         if First and then Ch in '-' | '+' then
            Neg := Ch = '-';
         elsif Ch in '0' .. '9' then
            Acc := Acc * 10 + Character'Pos (Ch) - Character'Pos ('0');
         else
            exit;
         end if;
         First := False;
      end loop;

      return (if Neg then -Acc else Acc);
   end To_Integer;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Petal_Value) return String is
   begin
      for I in reverse Value'Range loop
         if Value (I) /= ' ' then
            return String (Value (1 .. I));
         end if;
      end loop;
      return "";
   end To_String;

   --------------
   -- To_Value --
   --------------

   function To_Value (From : Boolean) return Petal_Value is
   begin
      return To_Value (if From then "true" else "false");
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (From : String) return Petal_Value is
      Result : String (1 .. Max_String_Length) := (others => ' ');
   begin
      Result (1 .. From'Length) := From;
      return Petal_Value (Result);
   end To_Value;

end Petal.Values;
