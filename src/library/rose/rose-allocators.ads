package Rose.Allocators is

   type Store (Capacity_Block_Ratio : Positive) is limited private;

   procedure Add_Available_Capacity
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive);

   function Allocate
     (Allocator : in out Store;
      Size      : Positive)
      return Natural;

private

   type Store (Capacity_Block_Ratio : Positive) is limited
      record
         null;
      end record;

end Rose.Allocators;
