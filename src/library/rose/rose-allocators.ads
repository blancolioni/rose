generic
   Log_2_Capacity : Natural;
package Rose.Allocators is

   type Store is limited private;

   procedure Add_Range
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive);
   --  Make the blocks Base .. Bound - 1 free, for example the space of a
   --  new backing store. The range need not be a power of two or
   --  aligned: it is freed as the largest aligned blocks that fit, so
   --  nothing outside it can be allocated.

   function Allocate
     (Allocator : in out Store;
      Size      : Positive)
      return Natural;

   procedure Deallocate
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive);
   --  Free a block that Allocate returned: Base is its index, and
   --  Bound - Base its size, which is rounded up to a power of two as
   --  Allocate rounds it. For any other range, use Add_Range.

private

   subtype Order_Type is Natural range 0 .. Log_2_Capacity - 1;
   type Free_Block_Array is array (1 .. 2 ** Log_2_Capacity) of Boolean
     with Pack;
   type Free_Block_Count is array (Order_Type) of Natural;

   type Store is limited
      record
         Free  : Free_Block_Array := (others => False);
         Count : Free_Block_Count := (others => 0);
      end record
   with Pack;

end Rose.Allocators;
