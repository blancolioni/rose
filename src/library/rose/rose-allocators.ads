generic
   Log_2_Capacity : Positive;
package Rose.Allocators
  with SPARK_Mode
is

   pragma Assertion_Policy (Pre => Ignore, Post => Ignore, Ghost => Ignore);
   --  The contracts are proved (make prove), not executed.

   pragma Compile_Time_Error
     (Log_2_Capacity > 30,
      "the block indices of a larger allocator do not fit in an Integer");

   Capacity : constant Positive := 2 ** (Log_2_Capacity - 1);
   --  The allocator holds the blocks 1 .. Capacity.

   type Store is limited private;

   function Rounded_Size (Size : Positive) return Positive
   with Ghost,
        Pre  => Size <= Capacity,
        Post => Rounded_Size'Result >= Size
                and then Rounded_Size'Result <= Capacity;
   --  Size rounded up to a power of two, which is the size of the block
   --  that Allocate gives for it.

   procedure Add_Range
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive);
   --  Make the blocks Base .. Bound - 1 free, for example the space of a
   --  new backing store. The range need not be a power of two or
   --  aligned: it is freed as the largest aligned blocks that fit, so
   --  nothing outside it can be allocated.  The allocator holds
   --  2 ** (Log_2_Capacity - 1) blocks: anything past them is ignored.

   procedure Allocate
     (Allocator : in out Store;
      Size      : Positive;
      Base      : out Natural)
   with Post => Base <= Capacity
                and then
                  (if Base > 0
                   then Size <= Capacity
                     and then (Base - 1) mod Rounded_Size (Size) = 0
                     and then Base - 1 <= Capacity - Rounded_Size (Size));
   --  Allocate a block of Size, rounded up to a power of two.  Base is its
   --  index, or 0 if there is no free block that large, which includes a
   --  Size larger than the allocator holds.  The block is aligned to its
   --  size, and inside the allocator.

   procedure Allocate_At
     (Allocator : in out Store;
      Base      : Positive;
      Size      : Positive;
      Success   : out Boolean)
   with Post => (if Success
                 then Size <= Capacity
                   and then (Base - 1) mod Rounded_Size (Size) = 0
                   and then Base - 1 <= Capacity - Rounded_Size (Size));
   --  Allocate the block that Allocate returned as Base for an allocation
   --  of Size, as when allocations are made again after a restart.
   --  Success is False if Base is not where such a block starts, or the
   --  block is not free.

   procedure Deallocate
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive)
   with Pre => Base <= Capacity
               and then Bound > Base
               and then Bound - Base <= Capacity;
   --  Free a block that Allocate returned: Base is its index, and
   --  Bound - Base its size, which is rounded up to a power of two as
   --  Allocate rounds it. For any other range, use Add_Range.

private

   subtype Order_Type is Natural range 0 .. Log_2_Capacity - 1;
   type Free_Block_Array is array (1 .. 2 ** Log_2_Capacity) of Boolean
     with Pack;

   type Store is limited
      record
         Free : Free_Block_Array := (others => False);
      end record;
   --  The blocks are the nodes of a binary tree, a level of it for each
   --  order: block 1 is all of the allocator, the children of block I are
   --  2I and 2I + 1, and the blocks 2 ** Order .. 2 ** (Order + 1) - 1 are
   --  those of order Order, so that Order_Type'Last is the order of the
   --  blocks of size 1.  Free (I) says that block I is free.

end Rose.Allocators;
