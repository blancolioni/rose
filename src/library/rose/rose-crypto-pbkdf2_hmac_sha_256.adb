with Rose.Crypto.HMAC_SHA_256;
with Rose.Crypto.SHA_256;

package body Rose.Crypto.PBKDF2_HMAC_SHA_256 is

   use System.Storage_Elements;

   ------------
   -- Derive --
   ------------

   procedure Derive
     (Password   : System.Storage_Elements.Storage_Array;
      Salt       : System.Storage_Elements.Storage_Array;
      Iterations : Positive;
      Key        : out System.Storage_Elements.Storage_Array)
   is
      Digest_Length : constant Storage_Count :=
                        Rose.Crypto.SHA_256.Digest_Length;
      Keyed         : Rose.Crypto.HMAC_SHA_256.Keyed;
      Block_Index   : Natural := 0;
      Next          : Storage_Offset := Key'First;
   begin
      Rose.Crypto.HMAC_SHA_256.Set_Key (Keyed, Password);

      --  Each block of the key is the xor of a chain of HMACs. The first
      --  is of the salt and the block's number, big-endian, and each of
      --  the others is of the one before.

      while Next <= Key'Last loop
         Block_Index := Block_Index + 1;

         declare
            First_Message : Storage_Array (1 .. Salt'Length + 4);
            U             : Rose.Crypto.SHA_256.Digest;
            V             : Rose.Crypto.SHA_256.Digest;
            T             : Rose.Crypto.SHA_256.Digest;
            Count         : constant Storage_Count :=
                              Storage_Count'Min
                                (Digest_Length, Key'Last - Next + 1);
         begin
            First_Message (1 .. Salt'Length) := Salt;
            declare
               Number : Natural := Block_Index;
            begin
               for I in reverse Storage_Offset range 1 .. 4 loop
                  First_Message (Salt'Length + I) :=
                    Storage_Element (Number mod 256);
                  Number := Number / 256;
               end loop;
            end;

            Rose.Crypto.HMAC_SHA_256.Authenticate
              (Keyed, First_Message, U);
            T := U;

            for J in 2 .. Iterations loop
               Rose.Crypto.HMAC_SHA_256.Authenticate (Keyed, U, V);
               U := V;
               for I in T'Range loop
                  T (I) := T (I) xor U (I);
               end loop;
            end loop;

            Key (Next .. Next + Count - 1) := T (1 .. Count);
            Next := Next + Count;
         end;
      end loop;
   end Derive;

end Rose.Crypto.PBKDF2_HMAC_SHA_256;
