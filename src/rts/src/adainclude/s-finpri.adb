------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . F I N A L I Z A T I O N _ P R I M I T I V E S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2023-2025, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Conversion;

package body System.Finalization_Primitives is

   function To_Collection_Node_Ptr is
     new Ada.Unchecked_Conversion (Address, Collection_Node_Ptr);

   function To_Collection_Ptr is
     new Ada.Unchecked_Conversion (Address, Finalization_Collection_Ptr);

   --  The run-time system is compiled with No_Unchecked_Access, so the
   --  references to a collection and its head are made from addresses.

   function Head_Of
     (Collection : Finalization_Collection) return Collection_Node_Ptr
   is (To_Collection_Node_Ptr (Collection.Head'Address));
   --  The dummy head of Collection

   procedure Detach_Node_From_Collection (Node : not null Collection_Node_Ptr);
   --  Remove a collection node from its associated finalization collection.
   --  Calls to the procedure with a Node that has already been detached have
   --  no effects.

   ---------------------------------
   -- Attach_Object_To_Collection --
   ---------------------------------

   procedure Attach_Object_To_Collection
     (Object_Address   : System.Address;
      Finalize_Address : not null Finalize_Address_Ptr;
      Collection       : in out Finalization_Collection)
   is
      Node : constant Collection_Node_Ptr :=
               To_Collection_Node_Ptr (Object_Address - Header_Size);

   begin
      --  Do not allow the attachment of controlled objects while the
      --  associated collection is being finalized.

      if Collection.Finalization_Started then
         raise Program_Error with "attachment after finalization started";
      end if;

      Node.Enclosing_Collection := To_Collection_Ptr (Collection'Address);
      Node.Finalize_Address     := Finalize_Address;
      Node.Prev                 := Head_Of (Collection);
      Node.Next                 := Collection.Head.Next;

      Collection.Head.Next.Prev := Node;
      Collection.Head.Next      := Node;
   end Attach_Object_To_Collection;

   -----------------------------
   -- Attach_Object_To_Master --
   -----------------------------

   procedure Attach_Object_To_Master
     (Object_Address   : System.Address;
      Finalize_Address : not null Finalize_Address_Ptr;
      Node             : not null Master_Node_Ptr;
      Master           : in out Finalization_Master)
   is
   begin
      Attach_Object_To_Node (Object_Address, Finalize_Address, Node.all);
      Chain_Node_To_Master (Node, Master);
   end Attach_Object_To_Master;

   ---------------------------
   -- Attach_Object_To_Node --
   ---------------------------

   procedure Attach_Object_To_Node
     (Object_Address   : System.Address;
      Finalize_Address : not null Finalize_Address_Ptr;
      Node             : in out Master_Node)
   is
   begin
      Node.Object_Address   := Object_Address;
      Node.Finalize_Address := Finalize_Address;
   end Attach_Object_To_Node;

   --------------------------
   -- Chain_Node_To_Master --
   --------------------------

   procedure Chain_Node_To_Master
     (Node   : not null Master_Node_Ptr;
      Master : in out Finalization_Master)
   is
   begin
      Node.Next   := Master.Head;
      Master.Head := Node;
   end Chain_Node_To_Master;

   ---------------------------------
   -- Detach_Node_From_Collection --
   ---------------------------------

   procedure Detach_Node_From_Collection
     (Node : not null Collection_Node_Ptr)
   is
   begin
      if Node.Prev /= null and then Node.Next /= null then
         Node.Prev.Next := Node.Next;
         Node.Next.Prev := Node.Prev;
         Node.Prev := null;
         Node.Next := null;
      end if;
   end Detach_Node_From_Collection;

   -----------------------------------
   -- Detach_Object_From_Collection --
   -----------------------------------

   procedure Detach_Object_From_Collection
     (Object_Address : System.Address)
   is
      Node : constant Collection_Node_Ptr :=
               To_Collection_Node_Ptr (Object_Address - Header_Size);

   begin
      Detach_Node_From_Collection (Node);
   end Detach_Object_From_Collection;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Collection : in out Finalization_Collection) is
      Curr_Ptr : Collection_Node_Ptr;
      Obj_Addr : Address;

      function Is_Empty_List (L : not null Collection_Node_Ptr) return Boolean;
      --  Determine whether a list contains only one element, the dummy head

      -------------------
      -- Is_Empty_List --
      -------------------

      function Is_Empty_List (L : not null Collection_Node_Ptr) return Boolean
      is
      begin
         return L.Next = L and then L.Prev = L;
      end Is_Empty_List;

   begin
      if Collection.Finalization_Started then

         --  Double finalization may occur during the handling of stand-alone
         --  libraries or the finalization of a pool with subpools.

         return;
      end if;

      Collection.Finalization_Started := True;

      --  Note that we cannot walk the list while finalizing its elements
      --  because the finalization of one may call Unchecked_Deallocation
      --  on another and, therefore, detach it from anywhere on the list.
      --  Instead, we empty the list by repeatedly finalizing the first
      --  element (after the dummy head) and detaching it from the list.

      while not Is_Empty_List (Head_Of (Collection)) loop
         Curr_Ptr := Collection.Head.Next;

         Detach_Node_From_Collection (Curr_Ptr);

         --  Skip the list header in order to offer proper object layout for
         --  finalization.

         Obj_Addr := Curr_Ptr.all'Address + Header_Size;

         Curr_Ptr.Finalize_Address (Obj_Addr);
      end loop;
   end Finalize;

   ---------------------
   -- Finalize_Master --
   ---------------------

   procedure Finalize_Master (Master : in out Finalization_Master) is
      Node : Master_Node_Ptr;

   begin
      Node := Master.Head;

      --  Without exception handlers, the finalization procedures are called
      --  without protection, whether or not Master.Exceptions_OK.

      while Node /= null loop
         Finalize_Object (Node.all, Node.Finalize_Address);

         Node := Node.Next;
      end loop;

      Master.Head := null;
   end Finalize_Master;

   ---------------------
   -- Finalize_Object --
   ---------------------

   procedure Finalize_Object
     (Node             : in out Master_Node;
      Finalize_Address : Finalize_Address_Ptr)
   is
      Addr : constant System.Address := Node.Object_Address;

   begin
      if Addr /= Null_Address then
         Node.Object_Address := Null_Address;

         Finalize_Address (Addr);
      end if;
   end Finalize_Object;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Collection : in out Finalization_Collection) is
   begin
      --  The dummy head must point to itself in both directions

      Collection.Head.Prev := Head_Of (Collection);
      Collection.Head.Next := Head_Of (Collection);

      Collection.Finalization_Started := False;
   end Initialize;

   -------------------------------------
   -- Suppress_Object_Finalize_At_End --
   -------------------------------------

   procedure Suppress_Object_Finalize_At_End (Node : in out Master_Node) is
   begin
      Node.Object_Address := Null_Address;
   end Suppress_Object_Finalize_At_End;

end System.Finalization_Primitives;
