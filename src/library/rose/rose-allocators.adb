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
      Order      : Order_Type := 0;
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
               return (Index - Start) * 2 ** Natural (Level);
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
         Allocator.Count (Level) := Allocator.Count (Level) - 1;
         Allocator.Free (Current_Index) := False;
         Allocator.Count (Level + 1) := Allocator.Count (Level + 1) + 2;
         Allocator.Free (Next_Index .. Next_Index + 1) := (True, True);
      end Split_At;

   begin
      while Alloc_Size < Size loop
         Order := Order + 1;
         Alloc_Size := Alloc_Size * 2;
      end loop;

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

      return Get_Free (Order);

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
      Order      : Order_Type := 0;
      Alloc_Size : Positive := 1;
      Index      : Positive;
   begin
      while Alloc_Size < Size loop
         Order := Order + 1;
         Alloc_Size := Alloc_Size * 2;
      end loop;

      Index := First_Free_Index (Order) + (Base - 1) / Alloc_Size;

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
