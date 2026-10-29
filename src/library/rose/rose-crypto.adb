package body Rose.Crypto is

   -----------
   -- Equal --
   -----------

   function Equal
     (Left, Right : System.Storage_Elements.Storage_Array)
      return Boolean
   is
      use System.Storage_Elements;
      Difference : Storage_Element := 0;
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;

      for I in 0 .. Storage_Offset'(Left'Length) - 1 loop
         Difference := Difference
           or (Left (Left'First + I) xor Right (Right'First + I));
      end loop;

      return Difference = 0;
   end Equal;

end Rose.Crypto;
