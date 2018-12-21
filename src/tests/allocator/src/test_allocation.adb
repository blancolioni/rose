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

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
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
   end Run_Tests;

end Test_Allocation;
