with Ada.Text_IO;

with Interfaces;

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

package body Tests.Signed is

   use type Interfaces.Integer_32;
   use type Interfaces.Integer_64;
   use type Rose.Words.Word_32;

   Values_32 : constant array (Positive range <>) of Interfaces.Integer_32 :=
     (0, 1, -1, 2, -2, 12_345, -12_345,
      Interfaces.Integer_32'Last - 1, Interfaces.Integer_32'Last,
      Interfaces.Integer_32'First + 1, Interfaces.Integer_32'First);

   Values_64 : constant array (Positive range <>) of Interfaces.Integer_64 :=
     (0, 1, -1, 2, -2, 12_345, -12_345,
      Interfaces.Integer_64 (Interfaces.Integer_32'Last),
      Interfaces.Integer_64 (Interfaces.Integer_32'First),
      Interfaces.Integer_64 (Interfaces.Integer_32'Last) + 1,
      Interfaces.Integer_64 (Interfaces.Integer_32'First) - 1,
      Interfaces.Integer_64'Last - 1, Interfaces.Integer_64'Last,
      Interfaces.Integer_64'First + 1, Interfaces.Integer_64'First);

   -----------------
   -- Signed_Test --
   -----------------

   function Signed_Test return Boolean is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Ada.Text_IO.Put ("Signed integer test");

      --  The word that goes into an invocation is the two's complement.

      Rose.System_Calls.Initialize_Send (Params, 0);
      Rose.System_Calls.Send_Integer_32 (Params, -1);
      Rose.System_Calls.Send_Integer_32
        (Params, Interfaces.Integer_32'First);
      if Rose.System_Calls.Get_Word_32 (Params, 0) /= 16#FFFF_FFFF#
        or else Rose.System_Calls.Get_Word_32 (Params, 1) /= 16#8000_0000#
      then
         return False;
      end if;

      for Value of Values_32 loop
         Rose.System_Calls.Initialize_Send (Params, 0);
         Rose.System_Calls.Send_Integer_32 (Params, Value);
         if Rose.System_Calls.Get_Integer_32 (Params, 0) /= Value then
            return False;
         end if;
      end loop;

      for Value of Values_64 loop
         Rose.System_Calls.Initialize_Send (Params, 0);
         Rose.System_Calls.Send_Integer_64 (Params, Value);
         if Rose.System_Calls.Get_Integer_64 (Params, 0) /= Value then
            return False;
         end if;
      end loop;

      --  A 32-bit value before a 64-bit one: the 64-bit value starts at
      --  the next word.

      Rose.System_Calls.Initialize_Send (Params, 0);
      Rose.System_Calls.Send_Integer_32 (Params, -7);
      Rose.System_Calls.Send_Integer_64
        (Params, Interfaces.Integer_64'First + 5);
      return Rose.System_Calls.Get_Integer_32 (Params, 0) = -7
        and then Rose.System_Calls.Get_Integer_64 (Params, 1)
                   = Interfaces.Integer_64'First + 5;
   end Signed_Test;

end Tests.Signed;
