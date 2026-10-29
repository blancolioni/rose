with Ada.Containers.Bounded_Doubly_Linked_Lists;
with Ada.Containers.Bounded_Holders;
with Ada.Containers.Bounded_Hashed_Maps;
with Ada.Containers.Bounded_Ordered_Maps;
with Ada.Containers.Bounded_Ordered_Sets;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Multiway_Trees;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Multisets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Text_IO;

package body Tests.Containers is

   pragma Unsuppress (All_Checks);
   --  The tests program is compiled with -gnatp, which would also turn off
   --  the containers' tampering checks: those are decided where a
   --  container is instantiated, by Container_Checks'Enabled and
   --  Tampering_Check'Enabled.

   package Integer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Integer);

   package Sorting is new Integer_Lists.Generic_Sorting;

   type Integer_Array is array (Positive range <>) of Integer;

   subtype Four is Positive range 1 .. 4;
   type Four_Integers is array (Four) of Integer;

   procedure Sort is
     new Ada.Containers.Generic_Array_Sort (Positive, Integer, Integer_Array);

   procedure Sort is
     new Ada.Containers.Generic_Constrained_Array_Sort
       (Four, Integer, Four_Integers);

   package Integer_Vectors is
     new Ada.Containers.Vectors (Positive, Integer);

   package Vector_Sorting is new Integer_Vectors.Generic_Sorting;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package String_Sorting is new String_Vectors.Generic_Sorting;

   subtype Key_Name is String (1 .. 4);

   package Name_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Key_Name,
        Element_Type    => Integer,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   package Integer_Maps is
     new Ada.Containers.Ordered_Maps (Integer, Integer);

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package String_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   package Ordered_String_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Integer);

   function Integer_Hash
     (Item : Integer)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Item));

   package Bounded_Lists is
     new Ada.Containers.Bounded_Doubly_Linked_Lists (Integer);

   package Bounded_Integer_Vectors is
     new Ada.Containers.Bounded_Vectors (Positive, Integer);

   package Bounded_Hashed_Integer_Maps is
     new Ada.Containers.Bounded_Hashed_Maps
       (Key_Type        => Integer,
        Element_Type    => Integer,
        Hash            => Integer_Hash,
        Equivalent_Keys => "=");

   package Bounded_Ordered_Integer_Maps is
     new Ada.Containers.Bounded_Ordered_Maps (Integer, Integer);

   Bounded_Capacity : constant := 4;

   package Integer_Sets is new Ada.Containers.Ordered_Sets (Integer);

   package Hashed_Integer_Sets is
     new Ada.Containers.Hashed_Sets
       (Element_Type        => Integer,
        Hash                => Integer_Hash,
        Equivalent_Elements => "=");

   package Integer_Multisets is
     new Ada.Containers.Ordered_Multisets (Integer);

   package String_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (String);

   package Bounded_Integer_Sets is
     new Ada.Containers.Bounded_Ordered_Sets (Integer);

   package String_Holders is
     new Ada.Containers.Indefinite_Holders (String);

   package Shapes is

      type Shape is tagged
         record
            Size : Natural := 0;
         end record;

      type Circle is new Shape with
         record
            Radius : Natural := 0;
         end record;

   end Shapes;
   --  A bounded holder holds a class-wide object; it cannot hold an
   --  unconstrained array such as String.

   package Small_Shape_Holders is
     new Ada.Containers.Bounded_Holders
       (Element_Type                 => Shapes.Shape'Class,
        Max_Size_In_Storage_Elements => 64,
        "="                          => Shapes."=");

   package Integer_Trees is
     new Ada.Containers.Multiway_Trees (Integer);

   package String_Trees is
     new Ada.Containers.Indefinite_Multiway_Trees (String);

   subtype Element_Value is Natural range 0 .. 3;

   Finalized : array (Element_Value) of Boolean := (others => False);
   --  Finalized (V) is set when an element with value V is finalized.

   package Tracked_Elements is

      type Tracked is new Ada.Finalization.Controlled with
         record
            Value : Element_Value := 0;
         end record;

      overriding procedure Finalize (Item : in out Tracked);

   end Tracked_Elements;

   package Tracked_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Tracked_Elements.Tracked, Tracked_Elements."=");

   function Array_Sort_Check return Boolean;
   --  True if Generic_Array_Sort and Generic_Constrained_Array_Sort sort
   --  an array.

   function Bounded_Check return Boolean;
   --  Check that each bounded form holds its capacity, and that one more
   --  element raises Capacity_Error; report the first thing that is
   --  wrong.

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   function Has_Elements
     (List     : Integer_Lists.List;
      Expected : Integer_Array)
      return Boolean;
   --  True if List holds Expected, in order. The list is walked forwards
   --  and backwards with cursors.

   function Holder_Check return Boolean;
   --  True if an indefinite and a bounded holder hold, replace and clear
   --  their elements.

   function Indefinite_Check return Boolean;
   --  Check an indefinite list, hashed map and ordered map of strings of
   --  different lengths, and report the first thing that is wrong.

   function Map_Check return Boolean;
   --  Check a hashed map and an ordered map, and report the first thing
   --  that is wrong.

   function Set_Check return Boolean;
   --  Check set operations, a multiset, an indefinite set and a bounded
   --  set, and report the first thing that is wrong.

   function Tamper_Check return Boolean;
   --  True if appending to a list while "for ... of" iterates over it
   --  raises Program_Error.

   function Tracked_Check return Boolean;
   --  True if the controlled elements of a list are finalized when the
   --  list is.

   function Tree_Check return Boolean;
   --  Check a multiway tree, built and walked depth first, and an
   --  indefinite one, and report the first thing that is wrong.

   function Vector_Check return Boolean;
   --  Check a vector of integers and a vector of strings, and report
   --  the first thing that is wrong.

   ----------------------
   -- Array_Sort_Check --
   ----------------------

   function Array_Sort_Check return Boolean is
      Items       : Integer_Array := (5, 2, 8, 1, 2);
      Constrained : Four_Integers := (4, -1, 3, 0);
   begin
      Sort (Items);
      Sort (Constrained);
      return Items = (1, 2, 2, 5, 8) and then Constrained = (-1, 0, 3, 4);
   end Array_Sort_Check;

   -------------------
   -- Bounded_Check --
   -------------------

   function Bounded_Check return Boolean is
      use type Ada.Containers.Count_Type;
      List    : Bounded_Lists.List (Capacity => Bounded_Capacity);
      Vector  : Bounded_Integer_Vectors.Vector (Capacity => Bounded_Capacity);
      Hashed  : Bounded_Hashed_Integer_Maps.Map
        (Capacity => Bounded_Capacity,
         Modulus  =>
           Bounded_Hashed_Integer_Maps.Default_Modulus (Bounded_Capacity));
      Ordered : Bounded_Ordered_Integer_Maps.Map
        (Capacity => Bounded_Capacity);
   begin
      for Value in 1 .. Bounded_Capacity loop
         List.Append (Value);
         Vector.Append (Value);
         Hashed.Insert (Value, Value);
         Ordered.Insert (Value, Value);
      end loop;

      if List.Length /= Bounded_Capacity
        or else Vector.Length /= Bounded_Capacity
        or else Hashed.Length /= Bounded_Capacity
        or else Ordered.Length /= Bounded_Capacity
        or else List.Last_Element /= Bounded_Capacity
        or else Vector.Last_Element /= Bounded_Capacity
        or else Hashed.Element (Bounded_Capacity) /= Bounded_Capacity
        or else Ordered.Last_Key /= Bounded_Capacity
      then
         return Fail ("bounded containers");
      end if;

      begin
         List.Append (0);
         return Fail ("bounded list over capacity");
      exception
         when Ada.Containers.Capacity_Error =>
            null;
      end;

      begin
         Vector.Append (0);
         return Fail ("bounded vector over capacity");
      exception
         when Ada.Containers.Capacity_Error =>
            null;
      end;

      begin
         Hashed.Insert (0, 0);
         return Fail ("bounded hashed map over capacity");
      exception
         when Ada.Containers.Capacity_Error =>
            null;
      end;

      begin
         Ordered.Insert (0, 0);
         return Fail ("bounded ordered map over capacity");
      exception
         when Ada.Containers.Capacity_Error =>
            null;
      end;

      return True;
   end Bounded_Check;

   --------------------
   -- Container_Test --
   --------------------

   function Container_Test return Boolean is
      use Integer_Lists;
      List     : Integer_Lists.List;
      Position : Cursor;
      Sum      : Integer := 0;
   begin
      Ada.Text_IO.Put ("Container test");

      List.Append (5);
      List.Append (3);
      List.Append (9);
      List.Append (1);
      List.Prepend (7);
      if not Has_Elements (List, (7, 5, 3, 9, 1)) then
         return Fail ("append and prepend");
      end if;

      Sorting.Sort (List);
      if not Sorting.Is_Sorted (List)
        or else not Has_Elements (List, (1, 3, 5, 7, 9))
      then
         return Fail ("sort");
      end if;

      for Item of List loop
         Sum := Sum + Item;
      end loop;
      if Sum /= 25 then
         return Fail ("for ... of");
      end if;

      Position := List.Find (5);
      if not Has_Element (Position) then
         return Fail ("find");
      end if;
      List.Insert (Before => Position, New_Item => 4);

      Position := List.Find (7);
      List.Delete (Position);
      if Has_Element (Position)
        or else List.Contains (7)
        or else not Has_Elements (List, (1, 3, 4, 5, 9))
      then
         return Fail ("insert and delete");
      end if;

      List.Reverse_Elements;
      if not Has_Elements (List, (9, 5, 4, 3, 1)) then
         return Fail ("reverse");
      end if;

      if not Tamper_Check then
         return Fail ("no tampering check");
      end if;

      if not Tracked_Check then
         return Fail ("elements not finalized");
      end if;

      if Boolean'Image (List.Is_Empty) /= "FALSE" then
         return Fail ("Boolean'Image");
      end if;

      if not Vector_Check then
         return False;
      end if;

      if not Array_Sort_Check then
         return Fail ("array sort");
      end if;

      if not Map_Check then
         return False;
      end if;

      if not Indefinite_Check or else not Bounded_Check then
         return False;
      end if;

      if not Set_Check or else not Tree_Check then
         return False;
      end if;

      if not Holder_Check then
         return Fail ("holders");
      end if;

      return True;
   end Container_Test;

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Message);
      return False;
   end Fail;

   ------------------
   -- Has_Elements --
   ------------------

   function Has_Elements
     (List     : Integer_Lists.List;
      Expected : Integer_Array)
      return Boolean
   is
      use Integer_Lists;
      use type Ada.Containers.Count_Type;
      Position : Cursor := List.First;
   begin
      if List.Length /= Expected'Length then
         return False;
      end if;

      for Item of Expected loop
         if Element (Position) /= Item then
            return False;
         end if;
         Next (Position);
      end loop;

      Position := List.Last;
      for Index in reverse Expected'Range loop
         if Element (Position) /= Expected (Index) then
            return False;
         end if;
         Previous (Position);
      end loop;

      return not Has_Element (Position);
   end Has_Elements;

   ------------------
   -- Holder_Check --
   ------------------

   function Holder_Check return Boolean is
      package Small renames Small_Shape_Holders;
      Holder : String_Holders.Holder := String_Holders.To_Holder ("abc");
      Shape  : Small.Holder := Small.To_Holder (Shapes.Shape'(Size => 1));
   begin
      if Holder.Element /= "abc"
        or else Small.Get (Shape) in Shapes.Circle
        or else Small.Get (Shape).Size /= 1
      then
         return False;
      end if;

      Holder.Replace_Element ("defgh");
      Small.Set (Shape, Shapes.Circle'(Size => 2, Radius => 3));
      if Holder.Element /= "defgh"
        or else Small.Get (Shape) not in Shapes.Circle
        or else Shapes.Circle (Small.Get (Shape)).Radius /= 3
      then
         return False;
      end if;

      Holder.Clear;
      return Holder.Is_Empty;
   end Holder_Check;

   ----------------------
   -- Indefinite_Check --
   ----------------------

   function Indefinite_Check return Boolean is
      use type Ada.Containers.Count_Type;
      Words   : String_Lists.List;
      Names   : String_Maps.Map;
      Lengths : Ordered_String_Maps.Map;
   begin
      Words.Append ("a");
      Words.Append ("bcd");
      Words.Prepend ("efghij");
      if Words.Length /= 3
        or else Words.First_Element /= "efghij"
        or else Words.Last_Element /= "bcd"
      then
         return Fail ("indefinite list");
      end if;

      Names.Insert ("one", "1");
      Names.Insert ("three", "333");
      Names.Replace ("one", "uno");
      if Names.Element ("three") /= "333"
        or else Names.Element ("one") /= "uno"
      then
         return Fail ("indefinite hashed map");
      end if;

      for Word of Words loop
         Lengths.Insert (Word, Word'Length);
      end loop;
      Lengths.Insert ("pear", 4);
      if Lengths.First_Key /= "a"
        or else Lengths.Last_Key /= "pear"
        or else Lengths.Element ("efghij") /= 6
      then
         return Fail ("indefinite ordered map");
      end if;

      return True;
   end Indefinite_Check;

   ---------------
   -- Map_Check --
   ---------------

   function Map_Check return Boolean is
      use type Ada.Containers.Count_Type;
      use type Name_Maps.Cursor;
      Names    : Name_Maps.Map;
      Numbers  : Integer_Maps.Map;
      Sum      : Integer := 0;
      Previous : Integer := Integer'First;

      function Generated (Index : Natural) return Key_Name
      is ('k'
          & Character'Val (Character'Pos ('0') + Index / 100 mod 10)
          & Character'Val (Character'Pos ('0') + Index / 10 mod 10)
          & Character'Val (Character'Pos ('0') + Index mod 10));
      --  A key for Index, from "k000" to "k999".

   begin
      Names.Insert ("pear", 1);
      Names.Insert ("plum", 2);
      Names.Insert ("kiwi", 3);
      Names.Insert ("lime", 4);
      Names.Replace ("plum", 20);
      Names.Delete ("kiwi");

      if Names.Length /= 3
        or else not Names.Contains ("pear")
        or else Names.Element ("plum") /= 20
        or else Names.Find ("kiwi") /= Name_Maps.No_Element
      then
         return Fail ("hashed map");
      end if;

      for Item of Names loop
         Sum := Sum + Item;
      end loop;
      if Sum /= 25 then
         return Fail ("hashed map iteration");
      end if;

      begin
         Names.Insert ("pear", 5);
         return Fail ("hashed map took a duplicate key");
      exception
         when Constraint_Error =>
            null;
      end;

      --  Many more keys make the map grow its table, and rehash.

      for Index in 0 .. 999 loop
         Names.Insert (Generated (Index), Index);
      end loop;
      if Names.Length /= 1_003
        or else Names.Element ("k123") /= 123
        or else Names.Element ("k999") /= 999
        or else Names.Element ("lime") /= 4
      then
         return Fail ("hashed map growth");
      end if;

      for Key of Integer_Array'(50, 10, 30, 20, 40) loop
         Numbers.Insert (Key, Key * 2);
      end loop;
      Numbers.Delete (30);

      for Position in Numbers.Iterate loop
         if Integer_Maps.Key (Position) <= Previous
           or else Integer_Maps.Element (Position)
                     /= 2 * Integer_Maps.Key (Position)
         then
            return Fail ("ordered map order");
         end if;
         Previous := Integer_Maps.Key (Position);
      end loop;

      if Numbers.Length /= 4
        or else Numbers.First_Key /= 10
        or else Numbers.Last_Key /= 50
        or else Integer_Maps.Key (Numbers.Floor (35)) /= 20
        or else Integer_Maps.Key (Numbers.Ceiling (35)) /= 40
      then
         return Fail ("ordered map");
      end if;

      begin
         for Item of Numbers loop
            Numbers.Include (Item + 1, Item);
         end loop;
         return Fail ("no ordered map tampering check");
      exception
         when Program_Error =>
            null;
      end;

      return True;
   end Map_Check;

   ---------------
   -- Set_Check --
   ---------------

   function Set_Check return Boolean is
      use type Ada.Containers.Count_Type;
      use type Integer_Sets.Set;
      A, B     : Integer_Sets.Set;
      Hashed   : Hashed_Integer_Sets.Set;
      Multiset : Integer_Multisets.Set;
      Words    : String_Sets.Set;
      Bounded  : Bounded_Integer_Sets.Set (Capacity => Bounded_Capacity);
      Sum      : Integer := 0;
      Previous : Integer := Integer'First;
   begin
      for Value in 1 .. 6 loop
         A.Insert (Value);
         B.Insert (Value + 3);
      end loop;

      if Integer_Sets.Union (A, B).Length /= 9
        or else Integer_Sets.Difference (A, B).Length /= 3
        or else Integer_Sets.Symmetric_Difference (A, B).Length /= 6
        or else not Integer_Sets.Is_Subset
                      (Integer_Sets.Intersection (A, B), A)
        or else (A and B) /= Integer_Sets.Intersection (A, B)
      then
         return Fail ("set operations");
      end if;

      for Item of Integer_Sets.Intersection (A, B) loop
         if Item <= Previous then
            return Fail ("ordered set order");
         end if;
         Previous := Item;
         Sum := Sum + Item;
      end loop;
      if Sum /= 15 then
         return Fail ("ordered set iteration");
      end if;

      for Value of Integer_Array'(3, 1, 3, 2, 1) loop
         Hashed.Include (Value);
         Multiset.Insert (Value);
      end loop;
      if Hashed.Length /= 3
        or else not Hashed.Contains (2)
        or else Multiset.Length /= 5
        or else Multiset.First_Element /= 1
        or else Multiset.Last_Element /= 3
      then
         return Fail ("hashed set or multiset");
      end if;

      Words.Insert ("pear");
      Words.Insert ("fig");
      Words.Insert ("banana");
      if Words.First_Element /= "banana" or else Words.Last_Element /= "pear"
      then
         return Fail ("indefinite set");
      end if;

      for Value in 1 .. Bounded_Capacity loop
         Bounded.Insert (Value);
      end loop;
      begin
         Bounded.Insert (0);
         return Fail ("bounded set over capacity");
      exception
         when Ada.Containers.Capacity_Error =>
            null;
      end;

      return True;
   end Set_Check;

   ------------------
   -- Tamper_Check --
   ------------------

   function Tamper_Check return Boolean is
      List : Integer_Lists.List;
   begin
      List.Append (1);
      List.Append (2);
      for Item of List loop
         List.Append (Item);
      end loop;
      return False;
   exception
      when Program_Error =>
         return True;
   end Tamper_Check;

   -------------------
   -- Tracked_Check --
   -------------------

   function Tracked_Check return Boolean is
   begin
      declare
         List : Tracked_Lists.List;
      begin
         for Value in 1 .. Element_Value'Last loop
            List.Append
              (Tracked_Elements.Tracked'
                 (Ada.Finalization.Controlled with Value => Value));
         end loop;

         --  The temporaries made by the appends are finalized by now:
         --  only the list's elements are left.

         Finalized := (others => False);
      end;

      return (for all Value in 1 .. Element_Value'Last =>
                Finalized (Value));
   end Tracked_Check;

   ----------------------
   -- Tracked_Elements --
   ----------------------

   package body Tracked_Elements is

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Item : in out Tracked) is
      begin
         Finalized (Item.Value) := True;
      end Finalize;

   end Tracked_Elements;

   ----------------
   -- Tree_Check --
   ----------------

   function Tree_Check return Boolean is
      use type Ada.Containers.Count_Type;
      use Integer_Trees;
      Tree     : Integer_Trees.Tree;
      First    : Integer_Trees.Cursor;
      Names    : String_Trees.Tree;
      Expected : constant Integer_Array := (1, 2, 3, 4);
      Next     : Positive := Expected'First;
   begin
      --  Root
      --  +- 1
      --  |  +- 2
      --  |  +- 3
      --  +- 4

      Tree.Append_Child (Tree.Root, 1);
      First := First_Child (Tree.Root);
      Tree.Append_Child (First, 2);
      Tree.Append_Child (First, 3);
      Tree.Append_Child (Tree.Root, 4);

      if Tree.Node_Count /= 5
        or else Child_Count (Tree.Root) /= 2
        or else Child_Count (First) /= 2
        or else not Is_Leaf (Last_Child (Tree.Root))
        or else Element (Last_Child (First)) /= 3
      then
         return Fail ("multiway tree");
      end if;

      for Item of Tree loop
         if Next > Expected'Last or else Item /= Expected (Next) then
            return Fail ("multiway tree order");
         end if;
         Next := Next + 1;
      end loop;

      Names.Append_Child (Names.Root, "a");
      Names.Append_Child (String_Trees.First_Child (Names.Root), "bcd");
      if String_Trees.Element
           (String_Trees.First_Child
              (String_Trees.First_Child (Names.Root))) /= "bcd"
      then
         return Fail ("indefinite multiway tree");
      end if;

      return Next = Expected'Last + 1;
   end Tree_Check;

   ------------------
   -- Vector_Check --
   ------------------

   function Vector_Check return Boolean is
      use type Ada.Containers.Count_Type;
      Vector  : Integer_Vectors.Vector;
      Strings : String_Vectors.Vector;
      Sum     : Integer := 0;
   begin
      for Value in reverse 1 .. 10 loop
         Vector.Append (Value);
      end loop;

      Vector_Sorting.Sort (Vector);
      if not Vector_Sorting.Is_Sorted (Vector)
        or else Vector.First_Element /= 1
        or else Vector.Last_Element /= 10
      then
         return Fail ("vector sort");
      end if;

      --  Insert moves the elements after it up by one, through
      --  overlapping storage.

      Vector.Insert (Before => 5, New_Item => 42);
      if Vector.Length /= 11
        or else Vector (4) /= 4
        or else Vector (5) /= 42
        or else (for some Index in 6 .. 11 =>
                   Vector (Index) /= Index - 1)
      then
         return Fail ("vector insert");
      end if;

      Vector.Delete (5);
      for Item of Vector loop
         Sum := Sum + Item;
      end loop;
      if Sum /= 55 or else Vector.Find_Index (7) /= 7 then
         return Fail ("vector delete and iteration");
      end if;

      Vector.Reverse_Elements;
      if Vector.First_Element /= 10
        or else Integer_Vectors.Element (Vector.Last) /= 1
      then
         return Fail ("vector reverse");
      end if;

      --  Growing well past the first capacity moves the elements to
      --  larger storage, more than once.

      for Value in 11 .. 2_000 loop
         Vector.Append (Value);
      end loop;
      if Vector.Length /= 2_000
        or else Vector.Capacity < Vector.Length
        or else Vector (1) /= 10
        or else Vector (2_000) /= 2_000
      then
         return Fail ("vector growth");
      end if;

      begin
         for Item of Vector loop
            Vector.Append (Item);
         end loop;
         return Fail ("no vector tampering check");
      exception
         when Program_Error =>
            null;
      end;

      Strings.Append ("pear");
      Strings.Append ("fig");
      Strings.Append ("banana");
      String_Sorting.Sort (Strings);
      if Strings.Length /= 3
        or else Strings (1) /= "banana"
        or else Strings (2) /= "fig"
        or else Strings (3) /= "pear"
      then
         return Fail ("indefinite vector");
      end if;

      Strings.Replace_Element (2, "elderberry");
      if Strings (2) /= "elderberry" or else Strings (3) /= "pear" then
         return Fail ("indefinite vector replace");
      end if;

      return True;
   end Vector_Check;

end Tests.Containers;
