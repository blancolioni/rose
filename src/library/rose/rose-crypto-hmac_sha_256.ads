with System.Storage_Elements;

with Rose.Crypto.SHA_256;

package Rose.Crypto.HMAC_SHA_256 is

   --  HMAC with SHA-256 (RFC 2104, FIPS 198-1).

   pragma Pure;

   type Keyed is private;
   --  A key, made ready to authenticate any number of messages.

   procedure Set_Key
     (Item : out Keyed;
      Key  : System.Storage_Elements.Storage_Array);

   procedure Authenticate
     (Item    : Keyed;
      Message : System.Storage_Elements.Storage_Array;
      Result  : out Rose.Crypto.SHA_256.Digest);

   function HMAC
     (Key     : System.Storage_Elements.Storage_Array;
      Message : System.Storage_Elements.Storage_Array)
      return Rose.Crypto.SHA_256.Digest;

private

   type Keyed is
      record
         Inner : Rose.Crypto.SHA_256.Context;
         Outer : Rose.Crypto.SHA_256.Context;
         --  The hashes of the padded key, xor'ed with the inner and outer
         --  pads, which every message starts from.
      end record;

end Rose.Crypto.HMAC_SHA_256;
