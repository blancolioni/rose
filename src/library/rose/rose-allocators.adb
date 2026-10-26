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
               Rose.Console_IO.Put_Line ("store: out of memory");
               return 0;
            end if;
         end;
      end loop;

      declare
         Index : constant Natural := Get_Free (Order);
         Result : constant Natural :=
                    (if Index = 0 then 0
                     else (Index - First_Free_Index (Order))
                     * 2 ** Natural (Order_Type'Last - Order)
                     + 1);
      begin
         if Index > 0 then
            Allocator.Free (Index) := False;
            Allocator.Count (Order) :=
              Allocator.Count (Order) - 1;
            return Result;
         else
            Rose.Console_IO.Put_Line ("store: expected a free slot");
            return 0;
         end if;
      end;

   end Allocate;

   -----------------
   -- Allocate_At --
   -----------------

   function Allocate_At
     (Allocator : in out Store;
      Base      : Positive;
      Size      : Positive)
      return Boolean
   is
      Order      : Order_Type := Order_Type'Last;
      Alloc_Size : Positive := 1;
      Index      : Positive;
      Level      : Order_Type;
      Block      : Positive;
   begin
      while Alloc_Size < Size loop
         Order := Order - 1;
         Alloc_Size := Alloc_Size * 2;
      end loop;

      if (Base - 1) mod Alloc_Size /= 0 then
         return False;
      end if;

      Index := First_Free_Index (Order) + (Base - 1) / Alloc_Size;

      --  The free block that holds it is the block itself, or the nearest
      --  of its ancestors: the parent of block I is I / 2.

      Level := Order;
      Block := Index;
      while not Allocator.Free (Block) loop
         if Level = 0 then
            return False;
         end if;
         Level := Level - 1;
         Block := Block / 2;
      end loop;

      --  Split that block down to this one, as Allocate does: the children
      --  of block I are 2I and 2I + 1.

      while Level < Order loop
         Allocator.Free (Block) := False;
         Allocator.Count (Level) := Allocator.Count (Level) - 1;
         Allocator.Free (2 * Block .. 2 * Block + 1) := (True, True);
         Allocator.Count (Level + 1) := Allocator.Count (Level + 1) + 2;
         Level := Level + 1;
         Block := Index / 2 ** Natural (Order - Level);
      end loop;

      Allocator.Free (Index) := False;
      Allocator.Count (Order) := Allocator.Count (Order) - 1;
      return True;
   end Allocate_At;

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

      --  The blocks of each order are a level of a binary tree, in which
      --  the parent of block I is I / 2, and its buddy is I xor 1. While
      --  the buddy is free, the two make a free block of the order
      --  above.

      loop
         exit when Order = 0;
         declare
            Buddy : constant Positive :=
              (if Index mod 2 = 0 then Index + 1 else Index - 1);
         begin
            exit when not Allocator.Free (Buddy);
            Allocator.Free (Buddy) := False;
            Allocator.Count (Order) := Allocator.Count (Order) - 1;
            Order := Order - 1;
            Index := Index / 2;
         end;
      end loop;

      Allocator.Free (Index) := True;
      Allocator.Count (Order) := Allocator.Count (Order) + 1;

   end Deallocate;

end Rose.Allocators;
