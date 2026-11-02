with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Equal_Case_Insensitive;
with Ada.Strings.Bounded.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Text_IO;

package body Tests.Strings is

   use Ada.Strings.Unbounded;

   package Short_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => 16);

   function Fixed_Ok return Boolean;
   function Bounded_Ok return Boolean;
   function Unbounded_Ok return Boolean;
   function Case_Insensitive_Ok return Boolean;

   ----------------
   -- Bounded_Ok --
   ----------------

   function Bounded_Ok return Boolean is
      use Short_Strings;
      Item  : Bounded_String := To_Bounded_String ("abc");
      Full  : Bounded_String;
      Valid : Boolean;
   begin
      Append (Item, "def");
      Valid := To_String (Item) = "abcdef"
        and then Length (Item) = 6
        and then Slice (Item, 2, 4) = "bcd"
        and then Element (Item, 1) = 'a'
        and then Item = "abcdef"
        and then Item < To_Bounded_String ("abd")
        and then Index (Item, "cd") = 3;

      Replace_Element (Item, 1, 'A');
      Valid := Valid and then To_String (Item) = "Abcdef";

      Replace_Slice (Item, 2, 3, "XY");
      Valid := Valid and then To_String (Item) = "AXYdef";

      --  Fill a bounded string to its maximum length. One more character
      --  is dropped from the right when asked, and is an error otherwise.

      Full := To_Bounded_String ("0123456789ABCDEF");
      Valid := Valid and then Length (Full) = Max_Length;
      Append (Full, "Z", Ada.Strings.Right);
      return Valid
        and then To_String (Full) = "0123456789ABCDEF";
   end Bounded_Ok;

   -------------------------
   -- Case_Insensitive_Ok --
   -------------------------

   function Case_Insensitive_Ok return Boolean is
      use type Ada.Containers.Hash_Type;

      function Bounded_Equal is
        new Ada.Strings.Bounded.Equal_Case_Insensitive (Short_Strings);
      function Bounded_Hash is
        new Ada.Strings.Bounded.Hash_Case_Insensitive (Short_Strings);

      package Name_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Natural,
         Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
         Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive);

      Names : Name_Maps.Map;
      Valid : Boolean;
   begin
      Valid := Ada.Strings.Equal_Case_Insensitive ("Hello", "hELLO")
        and then not Ada.Strings.Equal_Case_Insensitive ("Hello", "Hellp")
        and then not Ada.Strings.Equal_Case_Insensitive ("Hello", "Hell")
        and then Ada.Strings.Fixed.Equal_Case_Insensitive ("ABC", "abc")
        and then Ada.Strings.Hash_Case_Insensitive ("Hello")
                   = Ada.Strings.Hash_Case_Insensitive ("hELLO")
        and then Ada.Strings.Hash_Case_Insensitive ("Hello")
                   /= Ada.Strings.Hash_Case_Insensitive ("World");

      Valid := Valid
        and then Bounded_Equal
          (Short_Strings.To_Bounded_String ("Rose"),
           Short_Strings.To_Bounded_String ("rOSE"))
        and then not Bounded_Equal
          (Short_Strings.To_Bounded_String ("Rose"),
           Short_Strings.To_Bounded_String ("Roses"))
        and then Bounded_Hash (Short_Strings.To_Bounded_String ("Rose"))
                   = Bounded_Hash (Short_Strings.To_Bounded_String ("ROSE"));

      Valid := Valid
        and then Ada.Strings.Unbounded.Equal_Case_Insensitive
          (To_Unbounded_String ("Rose"), To_Unbounded_String ("rOSE"))
        and then not Ada.Strings.Unbounded.Equal_Case_Insensitive
          (To_Unbounded_String ("Rose"), To_Unbounded_String ("Roses"))
        and then Ada.Strings.Unbounded.Hash_Case_Insensitive
          (To_Unbounded_String ("Rose"))
            = Ada.Strings.Unbounded.Hash_Case_Insensitive
              (To_Unbounded_String ("ROSE"));

      --  Two spellings of one key find the same element.

      Names.Insert (To_Unbounded_String ("Putney"), 1);
      Names.Insert (To_Unbounded_String ("Rose"), 2);
      Names.Include (To_Unbounded_String ("PUTNEY"), 3);
      return Valid
        and then Natural (Names.Length) = 2
        and then Names.Element (To_Unbounded_String ("putney")) = 3
        and then Names.Contains (To_Unbounded_String ("rOSE"))
        and then not Names.Contains (To_Unbounded_String ("Roses"));
   end Case_Insensitive_Ok;

   --------------
   -- Fixed_Ok --
   --------------

   function Fixed_Ok return Boolean is
      package Fixed renames Ada.Strings.Fixed;
      Text : constant String := "  the quick brown fox  ";
   begin
      return Fixed.Index (Text, "brown") = 13
        and then Fixed.Index (Text, "slow") = 0
        and then Fixed.Count (Text, "o") = 2
        and then Fixed.Trim (Text, Ada.Strings.Both) = "the quick brown fox"
        and then Fixed.Trim (Text, Ada.Strings.Left) = "the quick brown fox  "
        and then Fixed.Head ("abc", 5) = "abc  "
        and then Fixed.Head ("abcdef", 3) = "abc"
        and then Fixed.Tail ("abcdef", 3) = "def"
        and then Fixed.Tail ("abc", 5, '.') = "..abc"
        and then Fixed.Replace_Slice ("abcdef", 2, 3, "XYZ") = "aXYZdef"
        and then Fixed.Insert ("abef", 3, "cd") = "abcdef"
        and then Fixed.Delete ("abcdef", 2, 4) = "aef"
        and then Fixed."*" (3, "ab") = "ababab"
        and then Fixed.Translate
          ("banana",
           Ada.Strings.Maps.To_Mapping ("an", "AN")) = "bANANA";
   end Fixed_Ok;

   -----------------
   -- String_Test --
   -----------------

   function String_Test return Boolean is
   begin
      Ada.Text_IO.Put ("String test");
      return Fixed_Ok and then Bounded_Ok and then Unbounded_Ok
        and then Case_Insensitive_Ok;
   end String_Test;

   -------------------
   -- Unbounded_Ok --
   -------------------

   function Unbounded_Ok return Boolean is
      Item  : Unbounded_String;
      Copy  : Unbounded_String;
      Valid : Boolean := True;
   begin
      --  Grow well past the first size, and check every character.

      for Index in 1 .. 5_000 loop
         Append (Item, Character'Val (Character'Pos ('a') + Index mod 26));
      end loop;
      Valid := Length (Item) = 5_000;
      for Index in 1 .. 5_000 loop
         if Element (Item, Index)
           /= Character'Val (Character'Pos ('a') + Index mod 26)
         then
            return False;
         end if;
      end loop;

      --  A copy shares nothing that matters: changing the copy leaves
      --  the original as it was.

      Copy := Item;
      Append (Copy, "tail");
      Replace_Element (Copy, 1, '#');
      Valid := Valid
        and then Length (Item) = 5_000
        and then Length (Copy) = 5_004
        and then Element (Item, 1) = 'b'
        and then Element (Copy, 1) = '#'
        and then Slice (Copy, 5_001, 5_004) = "tail";

      Item := To_Unbounded_String ("  hello, world  ");
      Valid := Valid
        and then Index (Item, "world") = 10
        and then Count (Item, "l") = 3
        and then To_String (Trim (Item, Ada.Strings.Both)) = "hello, world"
        and then Slice (Item, 3, 7) = "hello"
        and then Item = "  hello, world  "
        and then To_Unbounded_String ("abc") < To_Unbounded_String ("abd")
        and then To_String (Item & "!") = "  hello, world  !";

      Replace_Slice (Item, 3, 7, "goodbye");
      Valid := Valid and then To_String (Item) = "  goodbye, world  ";

      --  Many strings created and finalized, with their storage freed.

      for Round in 1 .. 2_000 loop
         declare
            Temp : Unbounded_String := To_Unbounded_String ("round ");
         begin
            Append (Temp, Integer'Image (Round));
            Copy := Temp;
            if Length (Copy) /= 6 + Integer'Image (Round)'Length then
               return False;
            end if;
         end;
      end loop;

      return Valid;
   end Unbounded_Ok;

end Tests.Strings;
