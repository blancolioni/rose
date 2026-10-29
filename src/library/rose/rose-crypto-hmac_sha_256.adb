package body Rose.Crypto.HMAC_SHA_256 is

   use System.Storage_Elements;

   ------------------
   -- Authenticate --
   ------------------

   procedure Authenticate
     (Item    : Keyed;
      Message : System.Storage_Elements.Storage_Array;
      Result  : out Rose.Crypto.SHA_256.Digest)
   is
      Inner  : Rose.Crypto.SHA_256.Context := Item.Inner;
      Outer  : Rose.Crypto.SHA_256.Context := Item.Outer;
      Digest : Rose.Crypto.SHA_256.Digest;
   begin
      Rose.Crypto.SHA_256.Update (Inner, Message);
      Rose.Crypto.SHA_256.Finish (Inner, Digest);
      Rose.Crypto.SHA_256.Update (Outer, Digest);
      Rose.Crypto.SHA_256.Finish (Outer, Result);
   end Authenticate;

   ----------
   -- HMAC --
   ----------

   function HMAC
     (Key     : System.Storage_Elements.Storage_Array;
      Message : System.Storage_Elements.Storage_Array)
      return Rose.Crypto.SHA_256.Digest
   is
      Item   : Keyed;
      Result : Rose.Crypto.SHA_256.Digest;
   begin
      Set_Key (Item, Key);
      Authenticate (Item, Message, Result);
      return Result;
   end HMAC;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key
     (Item : out Keyed;
      Key  : System.Storage_Elements.Storage_Array)
   is
      Block_Key : Storage_Array (1 .. Rose.Crypto.SHA_256.Block_Length) :=
                    (others => 0);
      Pad       : Storage_Array (Block_Key'Range);
   begin

      --  A key longer than a block is hashed first, and a shorter one is
      --  padded with zeros.

      if Key'Length > Block_Key'Length then
         Block_Key (1 .. Rose.Crypto.SHA_256.Digest_Length) :=
           Rose.Crypto.SHA_256.Hash (Key);
      else
         Block_Key (1 .. Key'Length) := Key;
      end if;

      for I in Pad'Range loop
         Pad (I) := Block_Key (I) xor 16#36#;
      end loop;
      Rose.Crypto.SHA_256.Initialize (Item.Inner);
      Rose.Crypto.SHA_256.Update (Item.Inner, Pad);

      for I in Pad'Range loop
         Pad (I) := Block_Key (I) xor 16#5C#;
      end loop;
      Rose.Crypto.SHA_256.Initialize (Item.Outer);
      Rose.Crypto.SHA_256.Update (Item.Outer, Pad);
   end Set_Key;

end Rose.Crypto.HMAC_SHA_256;
