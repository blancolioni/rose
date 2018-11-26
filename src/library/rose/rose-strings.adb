package body Rose.Strings is

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (Image : String)
      return Integer
   is
   begin
      return X : Integer := 0 do
         for Ch of Image loop
            if Ch in '0' .. '9' then
               X := X * 10 + Character'Pos (Ch) - 48;
            end if;
         end loop;
      end return;
   end To_Integer;

end Rose.Strings;
