generic
   Log_2_Capacity : Natural;
package Rose.Allocators is

   type Store is limited private;

   function Allocate
     (Allocator : in out Store;
      Size      : Positive)
      return Natural;

   procedure Deallocate
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive);

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
