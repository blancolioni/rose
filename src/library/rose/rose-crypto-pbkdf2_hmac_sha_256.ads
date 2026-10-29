with System.Storage_Elements;

package Rose.Crypto.PBKDF2_HMAC_SHA_256 is

   --  PBKDF2 with HMAC-SHA-256 as its pseudorandom function (RFC 8018,
   --  section 5.2): a key derived from a password, for keeping a hash of
   --  the password (#455).

   pragma Pure;

   procedure Derive
     (Password   : System.Storage_Elements.Storage_Array;
      Salt       : System.Storage_Elements.Storage_Array;
      Iterations : Positive;
      Key        : out System.Storage_Elements.Storage_Array);
   --  Fill Key with the key derived from Password and Salt. The more
   --  Iterations, the longer it takes to derive, and so to guess.

end Rose.Crypto.PBKDF2_HMAC_SHA_256;
