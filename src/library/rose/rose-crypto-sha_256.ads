private with Interfaces;

with System.Storage_Elements;

package Rose.Crypto.SHA_256 is

   --  SHA-256 (FIPS 180-4).

   pragma Pure;

   Digest_Length : constant := 32;
   Block_Length  : constant := 64;

   subtype Digest is
     System.Storage_Elements.Storage_Array (1 .. Digest_Length);

   type Context is private;
   --  A hash being computed: Initialize, Update with the data, and then
   --  Finish.

   procedure Initialize (Item : out Context);

   procedure Update
     (Item : in out Context;
      Data : System.Storage_Elements.Storage_Array);

   procedure Finish
     (Item   : in out Context;
      Result : out Digest);

   function Hash
     (Data : System.Storage_Elements.Storage_Array)
      return Digest;

private

   type State_Array is array (0 .. 7) of Interfaces.Unsigned_32;

   type Context is
      record
         State  : State_Array := (others => 0);
         Block  : System.Storage_Elements.Storage_Array (1 .. Block_Length) :=
                    (others => 0);
         Filled : System.Storage_Elements.Storage_Count := 0;
         Length : Interfaces.Unsigned_64 := 0;
         --  The number of bytes hashed so far.
      end record;

end Rose.Crypto.SHA_256;
