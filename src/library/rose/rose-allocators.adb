package body Rose.Allocators
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The loop invariants, assertions and lemmas are for the proof, and the
   --  boot servers' run-time system has no System.Assertions.

   subtype Power_Range is Natural range 0 .. 30;

   Pow : constant array (Power_Range) of Positive :=
     (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384,
      32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304,
      8388608, 16777216, 33554432, 67108864, 134217728, 268435456,
      536870912, 1073741824);
   --  2 ** Index.  The provers know a table of constants better than
   --  they know exponentiation.

   procedure Lemma_Double (Power : Power_Range)
   with Ghost,
        Pre  => Power < 30,
        Post => Pow (Power + 1) = 2 * Pow (Power);

   procedure Lemma_Larger (Small, Large : Power_Range)
   with Ghost,
        Pre  => Small <= Large,
        Post => Pow (Small) <= Pow (Large);

   procedure Lemma_Product (Left, Right : Power_Range)
   with Ghost,
        Pre  => Left + Right <= 30,
        Post => Pow (Left) * Pow (Right) = Pow (Left + Right);

   procedure Lemma_Multiple (Count : Natural; Size : Positive)
   with Ghost,
        Pre  => Long_Long_Integer (Count) * Long_Long_Integer (Size)
                <= Long_Long_Integer (Capacity),
        Post => (Count * Size) mod Size = 0;

   function Block_Size (Order : Order_Type) return Positive
   is (Pow (Order_Type'Last - Order));
   --  The number of blocks of size 1 that a block of order Order holds.

   function First_Free_Index (Level : Order_Type) return Positive
   is (Pow (Level));

   function Last_Free_Index (Level : Order_Type) return Positive
   is (Pow (Level + 1) - 1);

   function Get_Free
     (Allocator : Store;
      Level     : Order_Type)
      return Natural
   with Post => Get_Free'Result = 0
                or else Get_Free'Result in
                  First_Free_Index (Level) .. Last_Free_Index (Level);
   --  The first free block of the level, or 0 if there is none.

   function Order_For (Size : Positive) return Order_Type
   with Pre  => Size <= Capacity,
        Post => Block_Size (Order_For'Result) >= Size;
   --  The order of the smallest block that holds Size.

   ---------------
   -- Add_Range --
   ---------------

   procedure Add_Range
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive)
   is
      Last  : constant Positive := Positive'Min (Bound, Capacity + 1);
      --  Anything at or past Capacity + 1 is not in the allocator.
      Start : Positive := Base;
   begin
      while Start < Last loop
         pragma Loop_Variant (Increases => Start);

         declare
            Power : Natural := 0;
            Size  : Positive := 1;
         begin
            --  Grow the block while it stays aligned at Start and inside
            --  the range.
            while Power < Order_Type'Last
              and then (Start - 1) mod (Size * 2) = 0
              and then Start + Size * 2 <= Last
            loop
               pragma Loop_Invariant (Power <= Order_Type'Last);
               pragma Loop_Invariant (Size = Pow (Power));
               pragma Loop_Invariant (Start + Size <= Last);
               Lemma_Double (Power);
               Power := Power + 1;
               Size := Size * 2;
            end loop;

            Lemma_Larger (Power, Order_Type'Last);
            Deallocate (Allocator, Start, Start + Size);
            Start := Start + Size;
         end;
      end loop;
   end Add_Range;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Allocator : in out Store;
      Size      : Positive;
      Base      : out Natural)
   is
   begin
      Base := 0;

      if Size > Capacity then
         return;
      end if;

      declare
         Order : constant Order_Type := Order_For (Size);
      begin
         --  The smallest free block that is at least as large: the first
         --  free block of the nearest level, from Order up to the whole.

         for Found in reverse 0 .. Order loop
            declare
               Block : Natural := Get_Free (Allocator, Found);
               Level : Order_Type := Found;
            begin
               if Block > 0 then

                  --  Split it down to Order, keeping the left half of each
                  --  split, whose right half is free.

                  while Level < Order loop
                     pragma Loop_Invariant (Level < Order);
                     pragma Loop_Invariant
                       (Block in
                          First_Free_Index (Level) .. Last_Free_Index (Level));
                     Lemma_Double (Level);
                     Lemma_Double (Level + 1);
                     Lemma_Larger (Level + 2, Order_Type'Last + 1);
                     Allocator.Free (Block) := False;
                     Allocator.Free (2 * Block) := True;
                     Allocator.Free (2 * Block + 1) := True;
                     Level := Level + 1;
                     Block := 2 * Block;
                  end loop;

                  Lemma_Double (Order);
                  Lemma_Product (Order, Order_Type'Last - Order);
                  pragma Assert
                    (Block in
                       First_Free_Index (Order) .. Last_Free_Index (Order));
                  pragma Assert
                    ((Block - First_Free_Index (Order)) * Block_Size (Order)
                     <= (Pow (Order) - 1) * Block_Size (Order));
                  pragma Assert
                    ((Pow (Order) - 1) * Block_Size (Order)
                     = Capacity - Block_Size (Order));
                  Lemma_Multiple
                    (Block - First_Free_Index (Order), Block_Size (Order));
                  Allocator.Free (Block) := False;
                  Base := (Block - First_Free_Index (Order))
                    * Block_Size (Order) + 1;
                  pragma Assert (Rounded_Size (Size) = Block_Size (Order));
                  pragma Assert
                    (Base - 1
                     = (Block - First_Free_Index (Order))
                       * Block_Size (Order));
                  pragma Assert (Base - 1 <= Capacity - Block_Size (Order));
                  return;
               end if;
            end;
         end loop;
      end;
   end Allocate;

   -----------------
   -- Allocate_At --
   -----------------

   procedure Allocate_At
     (Allocator : in out Store;
      Base      : Positive;
      Size      : Positive;
      Success   : out Boolean)
   is
   begin
      Success := False;

      if Size > Capacity then
         return;
      end if;

      declare
         Order      : constant Order_Type := Order_For (Size);
         Alloc_Size : constant Positive := Block_Size (Order);
      begin
         if (Base - 1) mod Alloc_Size /= 0
           or else (Base - 1) / Alloc_Size >= First_Free_Index (Order)
         then
            return;
         end if;

         declare
            Index : constant Positive :=
              First_Free_Index (Order) + (Base - 1) / Alloc_Size;
            Level : Order_Type := Order;
            Block : Positive := Index;
         begin

            --  The free block that holds it is the block itself, or the
            --  nearest of its ancestors: the parent of block I is I / 2.

            while not Allocator.Free (Block) loop
               pragma Loop_Invariant
                 (Block in
                    First_Free_Index (Level) .. Last_Free_Index (Level));
               if Level = 0 then
                  return;
               end if;
               Level := Level - 1;
               Block := Block / 2;
            end loop;

            --  Split that block down to this one, as Allocate does: the
            --  children of block I are 2I and 2I + 1.

            while Level < Order loop
               pragma Loop_Invariant (Level < Order);
               pragma Loop_Invariant
                 (Block in
                    First_Free_Index (Level) .. Last_Free_Index (Level));
               Allocator.Free (Block) := False;
               Allocator.Free (2 * Block) := True;
               Allocator.Free (2 * Block + 1) := True;
               Level := Level + 1;
               Block := Index / Pow (Order - Level);
            end loop;

            Allocator.Free (Index) := False;
            Success := True;
         end;
      end;
   end Allocate_At;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive)
   is
      Order : constant Order_Type := Order_For (Bound - Base);
      Level : Order_Type := Order;
      Index : Positive :=
        First_Free_Index (Order) + (Base - 1) / Block_Size (Order);
   begin

      --  The parent of block I is I / 2, and its buddy is I xor 1.  While
      --  the buddy is free, the two make a free block of the level above.

      while Level > 0 loop
         pragma Loop_Invariant
           (Index in First_Free_Index (Level) .. Last_Free_Index (Level));

         declare
            Buddy : constant Positive :=
              (if Index mod 2 = 0 then Index + 1 else Index - 1);
         begin
            exit when not Allocator.Free (Buddy);
            Allocator.Free (Buddy) := False;
            Level := Level - 1;
            Index := Index / 2;
         end;
      end loop;

      Allocator.Free (Index) := True;
   end Deallocate;

   --------------
   -- Get_Free --
   --------------

   function Get_Free
     (Allocator : Store;
      Level     : Order_Type)
      return Natural
   is
   begin
      for Index in First_Free_Index (Level) .. Last_Free_Index (Level) loop
         if Allocator.Free (Index) then
            return Index;
         end if;
      end loop;

      return 0;
   end Get_Free;

   ------------------
   -- Lemma_Double --
   ------------------

   procedure Lemma_Double (Power : Power_Range) is null;

   ------------------
   -- Lemma_Larger --
   ------------------

   procedure Lemma_Larger (Small, Large : Power_Range) is null;

   --------------------
   -- Lemma_Multiple --
   --------------------

   procedure Lemma_Multiple (Count : Natural; Size : Positive) is null;

   -------------------
   -- Lemma_Product --
   -------------------

   procedure Lemma_Product (Left, Right : Power_Range) is null;

   ---------------
   -- Order_For --
   ---------------

   function Order_For (Size : Positive) return Order_Type is
      Order      : Order_Type := Order_Type'Last;
      Alloc_Size : Positive := 1;
   begin
      while Alloc_Size < Size loop
         pragma Loop_Variant (Decreases => Order);
         pragma Loop_Invariant
           (Alloc_Size = Pow (Order_Type'Last - Order));
         Order := Order - 1;
         Alloc_Size := Alloc_Size * 2;
      end loop;

      return Order;
   end Order_For;

   ------------------
   -- Rounded_Size --
   ------------------

   function Rounded_Size (Size : Positive) return Positive
   with Refined_Post => Rounded_Size'Result = Block_Size (Order_For (Size))
   is
   begin
      Lemma_Larger (Order_Type'Last - Order_For (Size), Order_Type'Last);
      return Block_Size (Order_For (Size));
   end Rounded_Size;

end Rose.Allocators;
