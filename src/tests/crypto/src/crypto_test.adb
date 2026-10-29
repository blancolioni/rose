with Ada.Command_Line;
with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Crypto.HMAC_SHA_256;
with Rose.Crypto.PBKDF2_HMAC_SHA_256;
with Rose.Crypto.SHA_256;

--  Check Rose.Crypto against published test vectors (#455): FIPS 180-4's
--  for SHA-256, RFC 4231's for HMAC-SHA-256, and the usual ones for
--  PBKDF2-HMAC-SHA-256. It runs on the host.

procedure Crypto_Test is

   use System.Storage_Elements;

   Failures : Natural := 0;

   function Bytes (Text : String) return Storage_Array;
   function Hex (Data : Storage_Array) return String;

   procedure Check
     (Name     : String;
      Result   : Storage_Array;
      Expected : String);

   -----------
   -- Bytes --
   -----------

   function Bytes (Text : String) return Storage_Array is
      Result : Storage_Array (1 .. Text'Length);
   begin
      for I in Text'Range loop
         Result (Storage_Offset (I - Text'First + 1)) :=
           Character'Pos (Text (I));
      end loop;
      return Result;
   end Bytes;

   -----------
   -- Check --
   -----------

   procedure Check
     (Name     : String;
      Result   : Storage_Array;
      Expected : String)
   is
      Actual : constant String := Hex (Result);
   begin
      if Actual = Expected then
         Ada.Text_IO.Put_Line ("PASS " & Name);
      else
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         Ada.Text_IO.Put_Line ("  expected " & Expected);
         Ada.Text_IO.Put_Line ("  actual   " & Actual);
         Failures := Failures + 1;
      end if;
   end Check;

   ---------
   -- Hex --
   ---------

   function Hex (Data : Storage_Array) return String is
      Digits_Of : constant String := "0123456789abcdef";
      Result    : String (1 .. 2 * Data'Length);
      Next      : Positive := 1;
   begin
      for Element of Data loop
         Result (Next) := Digits_Of (Natural (Element / 16) + 1);
         Result (Next + 1) := Digits_Of (Natural (Element mod 16) + 1);
         Next := Next + 2;
      end loop;
      return Result;
   end Hex;

   use Rose.Crypto;

begin
   Check ("sha-empty", SHA_256.Hash (Bytes ("")),
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
   Check ("sha-abc", SHA_256.Hash (Bytes ("abc")),
          "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
   Check ("sha-448",
          SHA_256.Hash
            (Bytes
               ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")),
          "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1");

   declare
      Item   : SHA_256.Context;
      Result : SHA_256.Digest;
      Chunk  : constant Storage_Array (1 .. 1000) :=
                 (others => Character'Pos ('a'));
   begin
      SHA_256.Initialize (Item);
      for I in 1 .. 1000 loop
         SHA_256.Update (Item, Chunk);
      end loop;
      SHA_256.Finish (Item, Result);
      Check ("sha-million-a", Result,
             "cdc76e5c9914fb9281a1c7e284d73e67"
             & "f1809a48a497200e046d39ccc7112cd0");
   end;

   Check ("hmac-1",
          HMAC_SHA_256.HMAC ((1 .. 20 => 16#0B#), Bytes ("Hi There")),
          "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7");
   Check ("hmac-2",
          HMAC_SHA_256.HMAC
            (Bytes ("Jefe"), Bytes ("what do ya want for nothing?")),
          "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843");
   Check ("hmac-6",
          HMAC_SHA_256.HMAC
            ((1 .. 131 => 16#AA#),
             Bytes ("Test Using Larger Than Block-Size Key - Hash Key First")),
          "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54");

   declare
      procedure Check_PBKDF2
        (Name       : String;
         Password   : String;
         Salt       : String;
         Iterations : Positive;
         Length     : Storage_Count;
         Expected   : String);

      ------------------
      -- Check_PBKDF2 --
      ------------------

      procedure Check_PBKDF2
        (Name       : String;
         Password   : String;
         Salt       : String;
         Iterations : Positive;
         Length     : Storage_Count;
         Expected   : String)
      is
         Key : Storage_Array (1 .. Length);
      begin
         PBKDF2_HMAC_SHA_256.Derive
           (Bytes (Password), Bytes (Salt), Iterations, Key);
         Check (Name, Key, Expected);
      end Check_PBKDF2;

   begin
      Check_PBKDF2
        ("pbkdf2-1", "password", "salt", 1, 32,
         "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b");
      Check_PBKDF2
        ("pbkdf2-2", "password", "salt", 2, 32,
         "ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43");
      Check_PBKDF2
        ("pbkdf2-4096", "password", "salt", 4096, 32,
         "c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a");
      Check_PBKDF2
        ("pbkdf2-64", "passwd", "salt", 1, 64,
         "55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc"
         & "49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783");
      Check_PBKDF2
        ("pbkdf2-40", "passwordPASSWORDpassword",
         "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096, 40,
         "348c89dbcbd32b2f32d814b8116e84cf2b17347ebc1800181c4e2a1fb8dd53e1"
         & "c635518c7dac47e9");
   end;

   Check ("equal",
          (1 => (if Equal (Bytes ("abc"), Bytes ("abc"))
                    and then not Equal (Bytes ("abc"), Bytes ("abd"))
                    and then not Equal (Bytes ("abc"), Bytes ("ab"))
                 then 1 else 0)),
          "01");

   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Crypto_Test;
