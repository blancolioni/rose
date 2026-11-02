package Tests.Strings is

   function String_Test return Boolean;
   --  Ada.Strings.Fixed, Bounded and Unbounded in user programs (#495):
   --  Fixed's Index, Count, Trim, Head, Tail, Replace_Slice, Insert,
   --  Delete, "*" and Translate; a bounded string appended to up to its
   --  maximum length, sliced, replaced and compared; and an unbounded
   --  string that grows well past its first size, is sliced, searched,
   --  trimmed, copied and replaced, with copies that do not change each
   --  other, and many of them created and finalized.
   --
   --  Equal_Case_Insensitive and Hash_Case_Insensitive for String,
   --  Bounded_String and Unbounded_String (#536): pairs that differ only in
   --  case, hashes that agree whatever the case, and a hashed map of
   --  unbounded strings in which two spellings of a key find one element.

end Tests.Strings;
