------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . S T O R A G E _ P O O L S . S U B P O O L S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  The Rose version (#359): only what the compiler needs to allocate and
--  free controlled objects on a storage pool, which is the whole of the
--  body of GNAT's Allocate_Any_Controlled and Deallocate_Any_Controlled
--  for a pool without subpools. Pools with subpools come with the update
--  of the run-time system to GCC 15 (#416).

with System.Finalization_Primitives;
with System.Storage_Elements;

package System.Storage_Pools.Subpools is
   pragma Preelaborate;

   type Root_Subpool is abstract tagged limited private;

   type Subpool_Handle is access all Root_Subpool'Class;
   for Subpool_Handle'Storage_Size use 0;
   --  Since subpools are limited types by definition, a handle is instead used
   --  to manage subpool abstractions.

   procedure Allocate_Any_Controlled
     (Pool          : in out Root_Storage_Pool'Class;
      Named_Subpool : Subpool_Handle;
      Collection    : in out
                        Finalization_Primitives.Finalization_Collection_Ptr;
      Addr          : out System.Address;
      Storage_Size  : System.Storage_Elements.Storage_Count;
      Alignment     : System.Storage_Elements.Storage_Count;
      Is_Controlled : Boolean;
      On_Subpool    : Boolean);
   --  Compiler interface. Allocate an object on Pool. If Is_Controlled, the
   --  object is preceded by the header of its finalization collection, and
   --  Addr is the address of the object, after the header. A subpool
   --  (Named_Subpool not null, or On_Subpool) raises Program_Error.

   procedure Deallocate_Any_Controlled
     (Pool          : in out Root_Storage_Pool'Class;
      Addr          : System.Address;
      Storage_Size  : System.Storage_Elements.Storage_Count;
      Alignment     : System.Storage_Elements.Storage_Count;
      Is_Controlled : Boolean);
   --  Compiler interface. Free an object allocated by
   --  Allocate_Any_Controlled.

private

   type Root_Subpool is abstract tagged limited null record;

end System.Storage_Pools.Subpools;
