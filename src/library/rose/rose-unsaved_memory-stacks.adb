with Ada.Exceptions;
with Interfaces;
with System.Machine_Code;

package body Rose.Unsaved_Memory.Stacks is

   use System.Storage_Elements;

   type Call_Frame is
      record
         Entry_Point : System.Address;
         Self        : System.Address;
         Stack_Top   : System.Address;
         Work        : Work_Procedure;
         Argument    : System.Address;
         Failed      : Boolean;
         Failure     : Ada.Exceptions.Exception_Occurrence;
      end record;
   --  The code that changes the stack reads the first three, at these
   --  offsets: the procedure to call on the new stack (Run), its
   --  argument (the frame's own address), and the address just past the
   --  stack's end. Run reads the rest.

   for Call_Frame use
      record
         Entry_Point at 0 range 0 .. 31;
         Self        at 4 range 0 .. 31;
         Stack_Top   at 8 range 0 .. 31;
      end record;

   procedure Run (Frame_Address : System.Address)
     with Convention => C;
   --  Call Work (Argument) for the Call_Frame at Frame_Address, and keep
   --  any exception it raises in the frame.

   ----------
   -- Call --
   ----------

   procedure Call
     (Work       : not null Work_Procedure;
      Argument   : System.Address;
      Stack_Size : System.Storage_Elements.Storage_Count :=
        Default_Stack_Size)
   is
      use System.Machine_Code;
      Base  : constant System.Address := Allocate (Stack_Size);
      Frame : Call_Frame;
      Done  : Interfaces.Unsigned_32;
      pragma Warnings (Off, Done);
      --  Only there because eax holds the frame's address and Run may
      --  change it.
   begin
      Frame.Entry_Point := Run'Address;
      Frame.Self := Frame'Address;
      Frame.Stack_Top := Base + Stack_Size;
      Frame.Work := Work;
      Frame.Argument := Argument;
      Frame.Failed := False;

      --  Keep the caller's stack pointer in esi, which Run keeps as it
      --  is (it is callee-saved), move to the new stack, aligned for
      --  the call as the i386 System V ABI asks, call Run with the
      --  frame, and move back.

      Asm ("movl %%esp, %%esi" & ASCII.LF & ASCII.HT
           & "movl 8(%%eax), %%esp" & ASCII.LF & ASCII.HT
           & "andl $-16, %%esp" & ASCII.LF & ASCII.HT
           & "subl $12, %%esp" & ASCII.LF & ASCII.HT
           & "pushl 4(%%eax)" & ASCII.LF & ASCII.HT
           & "call *(%%eax)" & ASCII.LF & ASCII.HT
           & "movl %%esi, %%esp",
           Outputs  => Interfaces.Unsigned_32'Asm_Output ("=a", Done),
           Inputs   => System.Address'Asm_Input ("0", Frame'Address),
           Clobber  => "ecx, edx, esi, memory, cc",
           Volatile => True);

      declare
         Stack : Storage_Array (1 .. Stack_Size)
           with Import, Volatile, Address => Base;
      begin
         Stack := (others => 0);
      end;
      Release (Base);

      if Frame.Failed then
         Ada.Exceptions.Reraise_Occurrence (Frame.Failure);
      end if;
   end Call;

   ---------
   -- Run --
   ---------

   procedure Run (Frame_Address : System.Address) is
      Frame : Call_Frame
        with Import, Address => Frame_Address;
   begin
      Frame.Work (Frame.Argument);
   exception
      when E : others =>
         Frame.Failed := True;
         Ada.Exceptions.Save_Occurrence (Frame.Failure, E);
   end Run;

end Rose.Unsaved_Memory.Stacks;
