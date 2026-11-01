------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . S T O R A G E _ P O O L S . S U B P O O L S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Storage_Elements; use System.Storage_Elements;

package body System.Storage_Pools.Subpools is

   function Header_Size_With_Padding
     (Alignment : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Count;
   --  Given an arbitrary alignment, calculate the size of the header which
   --  precedes a controlled object as the nearest multiple rounded up of the
   --  alignment.

   -----------------------------
   -- Allocate_Any_Controlled --
   -----------------------------

   procedure Allocate_Any_Controlled
     (Pool          : in out Root_Storage_Pool'Class;
      Named_Subpool : Subpool_Handle;
      Collection    : in out
                        Finalization_Primitives.Finalization_Collection_Ptr;
      Addr          : out System.Address;
      Storage_Size  : System.Storage_Elements.Storage_Count;
      Alignment     : System.Storage_Elements.Storage_Count;
      Is_Controlled : Boolean;
      On_Subpool    : Boolean)
   is
      pragma Unreferenced (Collection);

      N_Addr      : Address;
      N_Alignment : Storage_Count;
      N_Size      : Storage_Count;

      Header_And_Padding : Storage_Offset := 0;
      --  This offset includes the size of a header plus an additional padding
      --  due to a larger alignment of the object.

   begin
      if Named_Subpool /= null or else On_Subpool then
         raise Program_Error with "subpools are not supported";
      end if;

      if Is_Controlled then
         --  The size must account for the hidden header before the object.
         --  Account for possible padding space before the header due to a
         --  larger alignment of the object.

         Header_And_Padding := Header_Size_With_Padding (Alignment);

         N_Size := Storage_Size + Header_And_Padding;

         --  The alignment must account for the hidden header before the object

         N_Alignment :=
           System.Storage_Elements.Storage_Count'Max
             (Alignment, System.Finalization_Primitives.Header_Alignment);

      else
         N_Size      := Storage_Size;
         N_Alignment := Alignment;
      end if;

      Allocate (Pool, N_Addr, N_Size, N_Alignment);

      --  Move the address from the hidden list header to the start of the
      --  object. If there is padding due to larger alignment of the object,
      --  the padding is placed at the beginning.

      Addr := N_Addr + Header_And_Padding;
   end Allocate_Any_Controlled;

   -------------------------------
   -- Deallocate_Any_Controlled --
   -------------------------------

   procedure Deallocate_Any_Controlled
     (Pool          : in out Root_Storage_Pool'Class;
      Addr          : System.Address;
      Storage_Size  : System.Storage_Elements.Storage_Count;
      Alignment     : System.Storage_Elements.Storage_Count;
      Is_Controlled : Boolean)
   is
      N_Addr      : Address;
      N_Alignment : Storage_Count;
      N_Size      : Storage_Count;

      Header_And_Padding : Storage_Offset;
      --  This offset includes the size of a header plus an additional padding
      --  due to a larger alignment of the object.

   begin
      if Is_Controlled then
         Header_And_Padding := Header_Size_With_Padding (Alignment);

         --  Move the address from the object to the beginning of the header

         N_Addr := Addr - Header_And_Padding;

         --  The size of the deallocated object must include that of the header

         N_Size := Storage_Size + Header_And_Padding;

         --  The alignment must account for the hidden header before the object

         N_Alignment :=
           System.Storage_Elements.Storage_Count'Max
             (Alignment, System.Finalization_Primitives.Header_Alignment);

      else
         N_Addr      := Addr;
         N_Size      := Storage_Size;
         N_Alignment := Alignment;
      end if;

      Deallocate (Pool, N_Addr, N_Size, N_Alignment);
   end Deallocate_Any_Controlled;

   ------------------------------
   -- Header_Size_With_Padding --
   ------------------------------

   function Header_Size_With_Padding
     (Alignment : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Count
   is
      Size : constant Storage_Count :=
               System.Finalization_Primitives.Header_Size;

   begin
      if Size mod Alignment = 0 then
         return Size;

      --  Add enough padding to reach the nearest multiple of the alignment
      --  rounding up.

      else
         return ((Size + Alignment - 1) / Alignment) * Alignment;
      end if;
   end Header_Size_With_Padding;

end System.Storage_Pools.Subpools;
