with Rose.Console_IO;

package body Rose.Allocators is

   function First_Free_Index (Level : Order_Type) return Natural
   is (2 ** Natural (Level));

   function Last_Free_Index (Level : Order_Type) return Natural
   is (2 ** (Natural (Level) + 1) - 1);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Allocator : in out Store;
      Size      : Positive)
      return Natural
   is
      Order      : Order_Type := Order_Type'Last;
      Alloc_Size : Positive := 1;

      function Get_Free (Level : Order_Type) return Natural;
      procedure Split_At (Level : Order_Type);

      --------------
      -- Get_Free --
      --------------

      function Get_Free (Level : Order_Type) return Natural is
         Start : constant Natural := First_Free_Index (Level);
         Finish : constant Natural := Last_Free_Index (Level);
         Index  : Natural := Start;
      begin
         while Index <= Finish loop
            if Allocator.Free (Index) then
               return Index;
            end if;
            Index := Index + 1;
         end loop;
         return 0;
      end Get_Free;

      --------------
      -- Split_At --
      --------------

      procedure Split_At (Level : Order_Type) is
         Current_Start : constant Natural := First_Free_Index (Level);
         Current_Index : constant Natural := Get_Free (Level);
         Next_Start : constant Natural := First_Free_Index (Level + 1);
         Next_Index    : constant Natural :=
                           Next_Start + (Current_Index - Current_Start) * 2;
      begin
--           Rose.Console_IO.Put ("alloc: split: level = ");
--           Rose.Console_IO.Put (Natural (Level));
--           Rose.Console_IO.Put ("; start = ");
--           Rose.Console_IO.Put (Current_Start);
--           Rose.Console_IO.Put ("; index = ");
--           Rose.Console_IO.Put (Current_Index);
--           Rose.Console_IO.Put ("; count = ");
--           Rose.Console_IO.Put (Allocator.Count (Level));
--           Rose.Console_IO.Put ("; next-start = ");
--           Rose.Console_IO.Put (Next_Start);
--           Rose.Console_IO.Put ("; next-index = ");
--           Rose.Console_IO.Put (Next_Index);
--           Rose.Console_IO.New_Line;
         Allocator.Count (Level) := Allocator.Count (Level) - 1;
         Allocator.Free (Current_Index) := False;
         Allocator.Count (Level + 1) := Allocator.Count (Level + 1) + 2;
         Allocator.Free (Next_Index .. Next_Index + 1) := (True, True);
      end Split_At;

   begin
      while Alloc_Size < Size loop
         Order := Order - 1;
         Alloc_Size := Alloc_Size * 2;
      end loop;

--        Rose.Console_IO.Put ("alloc: size = ");
--        Rose.Console_IO.Put (Size);
--        Rose.Console_IO.Put ("; order = ");
--        Rose.Console_IO.Put (Natural (Order));
--        Rose.Console_IO.New_Line;

      while Allocator.Count (Order) = 0 loop
         declare
            Found : Boolean := False;
         begin
            for Split_Order in reverse 0 .. Order - 1 loop
               if Allocator.Count (Split_Order) > 0 then
                  Split_At (Split_Order);
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               return 0;
            end if;
         end;
      end loop;

      declare
         Index : constant Natural := Get_Free (Order);
      begin
         return (Index - First_Free_Index (Order))
           * 2 ** Natural (Order_Type'Last - Order)
           + 1;
      end;

   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Allocator : in out Store;
      Base      : Positive;
      Bound     : Positive)
   is
      Size       : constant Natural := Bound - Base;
      Order      : Order_Type := Order_Type'Last;
      Alloc_Size : Positive := 1;
      Index      : Positive;
   begin
      while Alloc_Size < Size loop
         Order := Order - 1;
         Alloc_Size := Alloc_Size * 2;
      end loop;

      Index := First_Free_Index (Order) + (Base - 1) / Alloc_Size;

      Rose.Console_IO.Put ("allocators: deallocate ");
      Rose.Console_IO.Put (Base);
      Rose.Console_IO.Put ("-");
      Rose.Console_IO.Put (Bound - 1);
      Rose.Console_IO.Put (" order=");
      Rose.Console_IO.Put (Order);
      Rose.Console_IO.Put (" size=");
      Rose.Console_IO.Put (Alloc_Size);
      Rose.Console_IO.Put (" index=");
      Rose.Console_IO.Put (Index);
      Rose.Console_IO.Put (" store-size=");
      Rose.Console_IO.Put (Natural (Allocator'Size) / 8);
      Rose.Console_IO.New_Line;

      loop
         Allocator.Free (Index) := True;
         Allocator.Count (Order) := Allocator.Count (Order) + 1;
         exit when Order = 0
           or else not Allocator.Free (Index + 1);

         Allocator.Count (Order) := Allocator.Count (Order) - 1;
         Order := Order - 1;
         Index := Index / 2;
      end loop;

   end Deallocate;

end Rose.Allocators;
