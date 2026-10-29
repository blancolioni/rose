--                              -*- Mode: Ada -*-
--  Filename        : s-secsta.ads
--  Description     : Secondary stack
--  Author          : Luke A. Guest
--  Created On      : Wed Nov 16 17:06:11 2016
--  Licence         : See LICENCE in the root directory.

--  The kernel's secondary stack: one static pool, rather than GNAT's
--  secondary stack, which allocates chunks from the heap. It provides the
--  subprograms that GCC 15 calls (#416), including SS_Allocate's alignment.

with System.Storage_Elements;

package System.Secondary_Stack is
   package SSE renames System.Storage_Elements;

   Default_Secondary_Stack_Size : Natural := 2 * 1024;  -- 2KB

   type Mark_Id is private;

   --  Allocate Storage_Size bytes on the stack, at an address that is a
   --  multiple of Alignment, and return the address in Addr.
   procedure SS_Allocate
     (Addr         : out System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count := Standard'Maximum_Alignment);

   --  Static stack, so we don't need to do anything here. In fact, it's,
   --  probably not even required.
   procedure SS_Free (Stk : in out System.Address);

   --  Return the current state (or top) of the stack.
   function SS_Mark return Mark_Id;

   --  Set the stack to M, freeing the last object allocated.
   procedure SS_Release (M : Mark_Id);
private
   type Mark_Id is new SSE.Storage_Count;

   SS_Size : constant Mark_Id := Mark_Id (Default_Secondary_Stack_Size);

   SS_First : constant Mark_Id := 0;
   SS_Top   : Mark_Id          := SS_First;

   type Pools is array (Mark_Id range <>) of aliased SSE.Storage_Element;

   SS_Pool  : Pools (SS_First .. SS_Size - 1)
     with Alignment => Standard'Maximum_Alignment;
end System.Secondary_Stack;
