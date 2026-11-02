with Rose.Containers.Bounded_Hashed_Maps;
with Rose.Words;

package Rose.Containers.Maps_Proof
  with SPARK_Mode
is

   --  gnatprove proves a generic through its instances: this is the one
   --  that isofs uses.

   function Hash (Key : Rose.Words.Word_32) return Hash_Type
   is (Hash_Type (Key));

   package Word_Maps is
     new Rose.Containers.Bounded_Hashed_Maps
       (Capacity     => 100,
        Modulus      => 317,
        Key_Type     => Rose.Words.Word_32,
        Element_Type => Rose.Words.Word_32,
        Hash         => Hash);

end Rose.Containers.Maps_Proof;
