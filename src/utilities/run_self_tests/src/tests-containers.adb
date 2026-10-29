with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Finalization;
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

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   function Has_Elements
     (List     : Integer_Lists.List;
      Expected : Integer_Array)
      return Boolean;
   --  True if List holds Expected, in order. The list is walked forwards
   --  and backwards with cursors.

   function Tamper_Check return Boolean;
   --  True if appending to a list while "for ... of" iterates over it
   --  raises Program_Error.

   function Tracked_Check return Boolean;
   --  True if the controlled elements of a list are finalized when the
   --  list is.

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
