package body Rose.Crypto.SHA_256 is

   use Interfaces;
   use System.Storage_Elements;

   K : constant array (0 .. 63) of Unsigned_32 :=
         (16#428A2F98#, 16#71374491#, 16#B5C0FBCF#, 16#E9B5DBA5#,
          16#3956C25B#, 16#59F111F1#, 16#923F82A4#, 16#AB1C5ED5#,
          16#D807AA98#, 16#12835B01#, 16#243185BE#, 16#550C7DC3#,
          16#72BE5D74#, 16#80DEB1FE#, 16#9BDC06A7#, 16#C19BF174#,
          16#E49B69C1#, 16#EFBE4786#, 16#0FC19DC6#, 16#240CA1CC#,
          16#2DE92C6F#, 16#4A7484AA#, 16#5CB0A9DC#, 16#76F988DA#,
          16#983E5152#, 16#A831C66D#, 16#B00327C8#, 16#BF597FC7#,
          16#C6E00BF3#, 16#D5A79147#, 16#06CA6351#, 16#14292967#,
          16#27B70A85#, 16#2E1B2138#, 16#4D2C6DFC#, 16#53380D13#,
          16#650A7354#, 16#766A0ABB#, 16#81C2C92E#, 16#92722C85#,
          16#A2BFE8A1#, 16#A81A664B#, 16#C24B8B70#, 16#C76C51A3#,
          16#D192E819#, 16#D6990624#, 16#F40E3585#, 16#106AA070#,
          16#19A4C116#, 16#1E376C08#, 16#2748774C#, 16#34B0BCB5#,
          16#391C0CB3#, 16#4ED8AA4A#, 16#5B9CCA4F#, 16#682E6FF3#,
          16#748F82EE#, 16#78A5636F#, 16#84C87814#, 16#8CC70208#,
          16#90BEFFFA#, 16#A4506CEB#, 16#BEF9A3F7#, 16#C67178F2#);

   Initial_State : constant State_Array :=
                     (16#6A09E667#, 16#BB67AE85#, 16#3C6EF372#,
                      16#A54FF53A#, 16#510E527F#, 16#9B05688C#,
                      16#1F83D9AB#, 16#5BE0CD19#);

   procedure Compress
     (State : in out State_Array;
      Block : Storage_Array);
   --  Mix the 64 bytes of Block into State.

   procedure Put_Byte
     (Item    : in out Context;
      Element : Storage_Element);
   --  Add one byte to the block, and compress the block when it is full.

   --------------
   -- Compress --
   --------------

   procedure Compress
     (State : in out State_Array;
      Block : Storage_Array)
   is
      W : array (0 .. 63) of Unsigned_32;
      A : Unsigned_32 := State (0);
      B : Unsigned_32 := State (1);
      C : Unsigned_32 := State (2);
      D : Unsigned_32 := State (3);
      E : Unsigned_32 := State (4);
      F : Unsigned_32 := State (5);
      G : Unsigned_32 := State (6);
      H : Unsigned_32 := State (7);
   begin
      for I in 0 .. 15 loop
         declare
            First : constant Storage_Offset :=
                      Block'First + Storage_Offset (4 * I);
         begin
            W (I) := Shift_Left (Unsigned_32 (Block (First)), 24)
              or Shift_Left (Unsigned_32 (Block (First + 1)), 16)
              or Shift_Left (Unsigned_32 (Block (First + 2)), 8)
              or Unsigned_32 (Block (First + 3));
         end;
      end loop;

      for I in 16 .. 63 loop
         declare
            S0 : constant Unsigned_32 :=
                   Rotate_Right (W (I - 15), 7)
                   xor Rotate_Right (W (I - 15), 18)
                   xor Shift_Right (W (I - 15), 3);
            S1 : constant Unsigned_32 :=
                   Rotate_Right (W (I - 2), 17)
                   xor Rotate_Right (W (I - 2), 19)
                   xor Shift_Right (W (I - 2), 10);
         begin
            W (I) := W (I - 16) + S0 + W (I - 7) + S1;
         end;
      end loop;

      for I in 0 .. 63 loop
         declare
            S1    : constant Unsigned_32 :=
                      Rotate_Right (E, 6) xor Rotate_Right (E, 11)
                      xor Rotate_Right (E, 25);
            Ch    : constant Unsigned_32 := (E and F) xor ((not E) and G);
            Temp1 : constant Unsigned_32 := H + S1 + Ch + K (I) + W (I);
            S0    : constant Unsigned_32 :=
                      Rotate_Right (A, 2) xor Rotate_Right (A, 13)
                      xor Rotate_Right (A, 22);
            Maj   : constant Unsigned_32 :=
                      (A and B) xor (A and C) xor (B and C);
            Temp2 : constant Unsigned_32 := S0 + Maj;
         begin
            H := G;
            G := F;
            F := E;
            E := D + Temp1;
            D := C;
            C := B;
            B := A;
            A := Temp1 + Temp2;
         end;
      end loop;

      State (0) := State (0) + A;
      State (1) := State (1) + B;
      State (2) := State (2) + C;
      State (3) := State (3) + D;
      State (4) := State (4) + E;
      State (5) := State (5) + F;
      State (6) := State (6) + G;
      State (7) := State (7) + H;
   end Compress;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (Item   : in out Context;
      Result : out Digest)
   is
      Bits : constant Unsigned_64 := Item.Length * 8;
   begin

      --  Padding: a one bit, zeros up to 8 bytes before the end of a
      --  block, and then the length in bits, big-endian.

      Put_Byte (Item, 16#80#);
      while Item.Filled /= Block_Length - 8 loop
         Put_Byte (Item, 0);
      end loop;

      for I in reverse 0 .. 7 loop
         Put_Byte
           (Item, Storage_Element (Shift_Right (Bits, 8 * I) and 16#FF#));
      end loop;

      for I in 0 .. 7 loop
         for J in 0 .. 3 loop
            Result (Storage_Offset (4 * I + J + 1)) :=
              Storage_Element
                (Shift_Right (Item.State (I), 8 * (3 - J)) and 16#FF#);
         end loop;
      end loop;
   end Finish;

   ----------
   -- Hash --
   ----------

   function Hash
     (Data : System.Storage_Elements.Storage_Array)
      return Digest
   is
      Item   : Context;
      Result : Digest;
   begin
      Initialize (Item);
      Update (Item, Data);
      Finish (Item, Result);
      return Result;
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item : out Context) is
   begin
      Item := (State  => Initial_State,
               Block  => (others => 0),
               Filled => 0,
               Length => 0);
   end Initialize;

   --------------
   -- Put_Byte --
   --------------

   procedure Put_Byte
     (Item    : in out Context;
      Element : Storage_Element)
   is
   begin
      Item.Filled := Item.Filled + 1;
      Item.Block (Item.Filled) := Element;
      if Item.Filled = Block_Length then
         Compress (Item.State, Item.Block);
         Item.Filled := 0;
      end if;
   end Put_Byte;

   ------------
   -- Update --
   ------------

   procedure Update
     (Item : in out Context;
      Data : System.Storage_Elements.Storage_Array)
   is
   begin
      for Element of Data loop
         Put_Byte (Item, Element);
      end loop;
      Item.Length := Item.Length + Unsigned_64 (Data'Length);
   end Update;

end Rose.Crypto.SHA_256;
