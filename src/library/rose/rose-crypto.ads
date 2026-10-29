with System.Storage_Elements;

package Rose.Crypto is

   --  Cryptographic hashes, for keeping password hashes (#455). Bytes are
   --  Storage_Elements.

   pragma Pure;

   function Equal
     (Left, Right : System.Storage_Elements.Storage_Array)
      return Boolean;
   --  True if Left and Right hold the same bytes. It takes the same time
   --  whichever bytes differ, so that comparing a hash does not tell how
   --  much of it was right. Arrays of different lengths are not equal.

end Rose.Crypto;
