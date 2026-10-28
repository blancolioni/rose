with Ada.Text_IO;

package body Tests.Secondary_Stack is

   function Filled (Length : Natural; Seed : Natural) return String;
   --  A string of Length letters, which depend on their index and Seed.
   --  It is returned on the secondary stack.

   function Is_Filled (Text : String; Seed : Natural) return Boolean;
   --  True if Text is what Filled returns for its length and Seed.

   ------------
   -- Filled --
   ------------

   function Filled (Length : Natural; Seed : Natural) return String is
      Result : String (1 .. Length);
   begin
      for I in Result'Range loop
         Result (I) :=
           Character'Val (Character'Pos ('a') + (I + Seed) mod 26);
      end loop;
      return Result;
   end Filled;

   ---------------
   -- Is_Filled --
   ---------------

   function Is_Filled (Text : String; Seed : Natural) return Boolean is
   begin
      for I in Text'Range loop
         if Text (I) /=
           Character'Val (Character'Pos ('a') + (I - Text'First + 1 + Seed)
                          mod 26)
         then
            return False;
         end if;
      end loop;
      return True;
   end Is_Filled;

   --------------------------
   -- Secondary_Stack_Test --
   --------------------------

   function Secondary_Stack_Test return Boolean is
   begin
      Ada.Text_IO.Put ("Secondary stack test");

      --  The first chunk holds 10 KB, so the second string needs a new
      --  chunk. Released and asked for again, the chunk is reused.

      for Round in 1 .. 2 loop
         declare
            Small : constant String := Filled (6_000, Round);
            Large : constant String := Filled (16_000, Round + 1);
         begin
            if not Is_Filled (Small, Round)
              or else not Is_Filled (Large, Round + 1)
            then
               return False;
            end if;
         end;
      end loop;

      --  Too large for either chunk: another one.

      declare
         Huge : constant String := Filled (30_000, 3);
      begin
         return Is_Filled (Huge, 3);
      end;
   end Secondary_Stack_Test;

end Tests.Secondary_Stack;
