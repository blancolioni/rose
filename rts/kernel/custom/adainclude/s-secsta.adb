--                              -*- Mode: Ada -*-
--  Filename        : s-secsta.adb
--  Description     : Secondary stack
--  Author          : Luke A. Guest
--  Created On      : Wed Nov 16 17:06:31 2016
--  Licence         : See LICENCE in the root directory.
package body System.Secondary_Stack is
   procedure SS_Allocate
     (Addr         : out System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count := Standard'Maximum_Alignment)
   is
      use type SSE.Integer_Address;

      Base  : constant SSE.Integer_Address :=
                SSE.To_Integer (SS_Pool (SS_First)'Address);
      Align : constant SSE.Integer_Address :=
                SSE.Integer_Address'Max (SSE.Integer_Address (Alignment), 1);
      Start : constant Mark_Id :=
                Mark_Id ((Base + SSE.Integer_Address (SS_Top) + Align - 1)
                         / Align * Align - Base);
   begin
      --  Make sure this won't go over the end of the stack.
      if Start + Mark_Id (Storage_Size) > SS_Size then
         raise Storage_Error;
      else
         Addr   := SS_Pool (Start)'Address;
         SS_Top := Start + Mark_Id (Storage_Size);
      end if;
   end SS_Allocate;

   procedure SS_Free (Stk : in out System.Address) is
   begin
      null;
   end SS_Free;

   function SS_Mark return Mark_Id is
   begin
      return SS_Top;
   end SS_Mark;

   procedure SS_Release (M : Mark_Id) is
   begin
      SS_Top := M;
   end SS_Release;
end System.Secondary_Stack;
