package Tests.Containers is

   function Container_Test return Boolean;
   --  Ada.Containers in user programs (#361, #440): a doubly-linked list
   --  of integers, sorted, iterated over with cursors and "for ... of",
   --  changed through cursors, and protected against tampering; and a
   --  list of controlled elements, which are finalized with the list.
   --  Vectors (#441): a vector of integers that grows past its first
   --  capacity, an indefinite vector of strings, and the array sorts.

end Tests.Containers;
