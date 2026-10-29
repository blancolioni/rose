package Tests.Containers is

   function Container_Test return Boolean;
   --  Ada.Containers in user programs (#361, #440): a doubly-linked list
   --  of integers, sorted, iterated over with cursors and "for ... of",
   --  changed through cursors, and protected against tampering; and a
   --  list of controlled elements, which are finalized with the list.
   --  Vectors (#441): a vector of integers that grows past its first
   --  capacity, an indefinite vector of strings, and the array sorts.
   --  Maps (#442): a hashed map with string keys that grows well past
   --  its first capacity, and an ordered map iterated in key order.
   --  Indefinite and bounded forms (#443): a list and maps of strings
   --  of different lengths, and each bounded form filled to its
   --  capacity.
   --  Sets, holders and trees (#444): set operations on ordered and
   --  hashed sets, an ordered multiset, an indefinite and a bounded set,
   --  an indefinite holder of strings and a bounded holder of a
   --  class-wide type, and a multiway tree walked depth first.

end Tests.Containers;
