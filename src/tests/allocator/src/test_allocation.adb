with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;

with Rose.Allocators;
with Rose.Console_IO;

package body Test_Allocation is

   package Allocators is new Rose.Allocators (20);

   Store : Allocators.Store;

   type Element_Type is
      record
         Index : Natural;
         Size  : Positive;
      end record;

   Dataset : array (1 .. 100) of Element_Type;

   package Small_Allocators is new Rose.Allocators (12);
   --  2048 blocks: room for the store's case, a 28 MB partition of
   --  16 KiB banks (1792 blocks), and small enough to fill.

   Capacity : constant := 2 ** 11;

   type Block_Map is array (1 .. Capacity) of Boolean;

   Failures : Natural := 0;

   procedure Check (Name : String; Passed : Boolean);
   --  Report a test.

   procedure Fill
     (Allocator : in out Small_Allocators.Store;
      Size      : Positive;
      Base      : Positive;
      Bound     : Positive;
      Count     : out Natural;
      Inside    : out Boolean);
   --  Allocate blocks of Size until none is left. Count is the number
   --  allocated, and Inside is True if every one lies in Base .. Bound - 1
   --  and none overlaps another.

   procedure Test_Merge;
   procedure Test_One_MiB_Blocks;
   procedure Test_Random_Use;
   procedure Test_Smoke;
   procedure Test_Store_Partition;
   procedure Test_Unaligned_Range;

   -----------
   -- Check --
   -----------

   procedure Check (Name : String; Passed : Boolean) is
   begin
      if Passed then
         Rose.Console_IO.Put_Line ("PASS " & Name);
      else
         Rose.Console_IO.Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Allocator : in out Small_Allocators.Store;
      Size      : Positive;
      Base      : Positive;
      Bound     : Positive;
      Count     : out Natural;
      Inside    : out Boolean)
   is
      Used : Block_Map := (others => False);
   begin
      Count := 0;
      Inside := True;
      loop
         declare
            Index : constant Natural :=
              Small_Allocators.Allocate (Allocator, Size);
         begin
            exit when Index = 0;
            Count := Count + 1;
            if Index < Base or else Index + Size > Bound then
               Inside := False;
            else
               for I in Index .. Index + Size - 1 loop
                  if Used (I) then
                     Inside := False;
                  end if;
                  Used (I) := True;
               end loop;
            end if;
         end;
      end loop;
   end Fill;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Test_Smoke;
      Test_Store_Partition;
      Test_One_MiB_Blocks;
      Test_Unaligned_Range;
      Test_Merge;
      Test_Random_Use;

      if Failures > 0 then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Run_Tests;

   ----------------
   -- Test_Merge --
   ----------------

   --  Freeing every single block of a range lets the largest blocks that
   --  fit in it be allocated again: 1792 = 1024 + 512 + 256.

   procedure Test_Merge is
      Allocator : Small_Allocators.Store;
      Count     : Natural;
      Inside    : Boolean;
   begin
      Small_Allocators.Add_Range (Allocator, 1, 1793);
      Fill (Allocator, 1, 1, 1793, Count, Inside);
      for I in 1 .. 1792 loop
         Small_Allocators.Deallocate (Allocator, I, I + 1);
      end loop;
      Check ("merge: 1024, 512 and 256 again",
             Small_Allocators.Allocate (Allocator, 1024) /= 0
             and then Small_Allocators.Allocate (Allocator, 512) /= 0
             and then Small_Allocators.Allocate (Allocator, 256) /= 0
             and then Small_Allocators.Allocate (Allocator, 1) = 0);
   end Test_Merge;

   -------------------------
   -- Test_One_MiB_Blocks --
   -------------------------

   --  A 1 MiB reservation is 64 banks, so the 28 MB partition holds 28.

   procedure Test_One_MiB_Blocks is
      Allocator : Small_Allocators.Store;
      Count     : Natural;
      Inside    : Boolean;
   begin
      Small_Allocators.Add_Range (Allocator, 1, 1793);
      Fill (Allocator, 64, 1, 1793, Count, Inside);
      Check ("1 MiB blocks: 28 in 28 MB", Count = 28 and then Inside);
   end Test_One_MiB_Blocks;

   ---------------------
   -- Test_Random_Use --
   ---------------------

   --  Allocate and free blocks of random sizes, as the store does: it
   --  frees a block with the size it asked for, and Allocate gives it a
   --  block of the next power of two. No block may overlap another that
   --  is still allocated, or lie outside the range.

   procedure Test_Random_Use is
      subtype Request is Positive range 1 .. 64;
      package Random_Sizes is new Ada.Numerics.Discrete_Random (Request);
      package Random_Choices is new Ada.Numerics.Discrete_Random (Boolean);

      type Held_Block is
         record
            Index : Natural := 0;
            Size  : Positive := 1;
         end record;

      Allocator : Small_Allocators.Store;
      Sizes     : Random_Sizes.Generator;
      Choices   : Random_Choices.Generator;
      Held      : array (1 .. 1792) of Held_Block;
      Held_Last : Natural := 0;
      Owner     : array (1 .. Capacity) of Natural := (others => 0);
      Passed    : Boolean := True;

      function Rounded (Size : Positive) return Positive;

      -------------
      -- Rounded --
      -------------

      function Rounded (Size : Positive) return Positive is
         Result : Positive := 1;
      begin
         while Result < Size loop
            Result := Result * 2;
         end loop;
         return Result;
      end Rounded;

   begin
      Random_Sizes.Reset (Sizes, 249);
      Random_Choices.Reset (Choices, 249);
      Small_Allocators.Add_Range (Allocator, 1, 1793);

      for Step in 1 .. 20_000 loop
         if Held_Last = 0 or else Random_Choices.Random (Choices) then
            declare
               Size  : constant Positive := Random_Sizes.Random (Sizes);
               Index : constant Natural :=
                 Small_Allocators.Allocate (Allocator, Size);
            begin
               if Index /= 0 then
                  if Index + Rounded (Size) > 1793 then
                     Passed := False;
                  else
                     for I in Index .. Index + Rounded (Size) - 1 loop
                        if Owner (I) /= 0 then
                           Passed := False;
                        end if;
                        Owner (I) := Step;
                     end loop;
                  end if;
                  Held_Last := Held_Last + 1;
                  Held (Held_Last) := (Index, Size);
               end if;
            end;
         else
            declare
               Victim : constant Positive :=
                 Random_Sizes.Random (Sizes) mod Held_Last + 1;
               Item   : constant Held_Block := Held (Victim);
            begin
               Small_Allocators.Deallocate
                 (Allocator, Item.Index, Item.Index + Item.Size);
               for I in Item.Index .. Item.Index + Rounded (Item.Size) - 1
               loop
                  if I in Owner'Range then
                     Owner (I) := 0;
                  end if;
               end loop;
               Held (Victim) := Held (Held_Last);
               Held_Last := Held_Last - 1;
            end;
         end if;

         exit when not Passed;
      end loop;

      Check ("random use: no overlapping blocks", Passed);
   end Test_Random_Use;

   ----------------
   -- Test_Smoke --
   ----------------

   procedure Test_Smoke is
   begin
      Allocators.Deallocate (Store, 1, 2 ** 19);
      for I in Dataset'Range loop
         Dataset (I).Size := 2;
         Dataset (I).Index :=
           Allocators.Allocate (Store, Dataset (I).Size);
         if Dataset (I).Index = 0 then
            Rose.Console_IO.Put_Line
              ("test: out of memory after" & I'Image & " allocations");
            exit;
         end if;
      end loop;
      Check ("smoke: 100 allocations",
             (for all Element of Dataset => Element.Index /= 0));
   end Test_Smoke;

   --------------------------
   -- Test_Store_Partition --
   --------------------------

   --  The store's partition is 1792 banks, which is not a power of two.
   --  Deallocate rounded it up to 2048 (#249); Add_Range gives exactly
   --  1792, all inside the partition.

   procedure Test_Store_Partition is
      Allocator : Small_Allocators.Store;
      Count     : Natural;
      Inside    : Boolean;
   begin
      Small_Allocators.Add_Range (Allocator, 1, 1793);
      Fill (Allocator, 1, 1, 1793, Count, Inside);
      Check ("store partition: exactly 1792 blocks",
             Count = 1792 and then Inside);
   end Test_Store_Partition;

   --------------------------
   -- Test_Unaligned_Range --
   --------------------------

   --  A range that starts and ends off any power-of-two boundary.

   procedure Test_Unaligned_Range is
      Allocator : Small_Allocators.Store;
      Count     : Natural;
      Inside    : Boolean;
   begin
      Small_Allocators.Add_Range (Allocator, 3, 1000);
      Fill (Allocator, 1, 3, 1000, Count, Inside);
      Check ("unaligned range: 997 blocks, all inside",
             Count = 997 and then Inside);
   end Test_Unaligned_Range;

end Test_Allocation;
