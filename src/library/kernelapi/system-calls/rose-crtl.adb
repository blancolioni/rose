with Rose.Capabilities.Standard;
with Rose.Interfaces.Heap;
with Rose.Interfaces.Process;
with Rose.Invocation;
with Rose.Limits;
with Rose.System_Calls;
with Rose.Words;

package body Rose.CRTL is

   use System.Storage_Elements;
   use type System.Address;

   type GCC_Exception_Access is access all Integer;

   type Unwind_Action is new Integer;

   type Exception_Class is new Integer;

   type Unwind_Reason_Code is new Integer;

   Heap_Alignment : constant := Standard'Maximum_Alignment;

   --  The heap is the process's resizable heap segment, which elf adds
   --  (#358). It holds blocks one after another, from Heap_Base up to
   --  Heap_Top, and grows with the Heap interface's Request_New_Bound when
   --  no free block is big enough. The first allocation gets the Heap
   --  interface from the process's own Process interface, which a process
   --  that exec starts has as cap 1. Adjacent free blocks are merged when
   --  a search for a free block passes them.

   type Block_Header is record
      Size : Storage_Count;
      --  The size of the block, its header included: a multiple of
      --  Heap_Alignment.
      Free : Boolean;
   end record;

   Header_Size : constant Storage_Count :=
     (Block_Header'Max_Size_In_Storage_Elements + Heap_Alignment - 1)
     / Heap_Alignment * Heap_Alignment;
   --  A block's header takes whole alignment units, so the data that
   --  follows it is aligned.

   Min_Block_Size : constant Storage_Count := Header_Size + Heap_Alignment;
   --  The smallest block: a split leaves no free block smaller than this.

   Heap_Started      : Boolean := False;
   Have_Heap         : Boolean := False;
   Heap_Base         : System.Address := System.Null_Address;
   Heap_Top          : System.Address := System.Null_Address;
   Current_Bound_Cap : Rose.Capabilities.Capability := 0;
   Request_Bound_Cap : Rose.Capabilities.Capability := 0;

   procedure Start_Heap;
   --  Get the caps of the Heap interface, and the heap's current top.
   --  Have_Heap is False if there is no heap.

   function Current_Bound return System.Address;
   --  The top of the heap segment, from the Heap interface.

   function Grow_Heap (Size : Storage_Count) return Boolean;
   --  Grow the heap so that its last block is free and at least Size
   --  long. False if the heap cannot grow that far.

   function Header (Block : System.Address) return Block_Header;
   procedure Set_Header
     (Block : System.Address;
      Size  : Storage_Count;
      Free  : Boolean);

   function Merge_Free (Block : System.Address) return Storage_Count;
   --  Block is free: merge it with the free blocks that follow it, and
   --  return its new size.

   function Block_Of (Ptr : System.Address) return System.Address
   is (Ptr - Header_Size);
   --  The block whose data starts at Ptr.

   Exception_Tracebacks : Integer := 0;
   pragma Export (C, Exception_Tracebacks, "__gl_exception_tracebacks");

   WC_Encoding : Character := ' ';
   pragma Export (C, WC_Encoding, "__gl_wc_encoding");

   function Get_Executable_Load_Address return System.Address
   is (System.Null_Address);

   pragma Export (C, Get_Executable_Load_Address,
                  "__gnat_get_executable_load_address");

   procedure Unwind_DeleteException (Excp : System.Address) is null;
   pragma Export (C, Unwind_DeleteException, "_Unwind_DeleteException");

   procedure Unwind_RaiseException
     (UW_Exception : not null GCC_Exception_Access)
   is null;

   pragma Export (C, Unwind_RaiseException, "__gnat_Unwind_RaiseException");

   procedure Unwind_ForcedUnwind
     (UW_Exception : not null GCC_Exception_Access;
      UW_Handler   : System.Address;
      UW_Argument  : System.Address)
   is null;
   pragma Export (C, Unwind_ForcedUnwind, "__gnat_Unwind_ForcedUnwind");

   function CleanupUnwind_Handler
     (UW_Version   : Integer;
      UW_Phases    : Unwind_Action;
      UW_Eclass    : Exception_Class;
      UW_Exception : not null GCC_Exception_Access;
      UW_Context   : System.Address;
      UW_Argument  : System.Address) return Unwind_Reason_Code;
   pragma Export (C, CleanupUnwind_Handler,
                  "__gnat_cleanupunwind_handler");

   function Backtrace
     (Traceback   : System.Address;
      Len         : Integer;
      Exclude_Min : System.Address;
      Exclude_Max : System.Address;
      Skip_Frames : Integer)
      return        Integer;
   pragma Export (C, Backtrace, "__gnat_backtrace");

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Size : System.Storage_Elements.Storage_Count)
     return System.Address
   is
      Max_Size : constant Storage_Count :=
        Storage_Count'Last - Header_Size - Heap_Alignment;
   begin
      if Size = 0 or else Size > Max_Size then
         return System.Null_Address;
      end if;

      if not Heap_Started then
         Start_Heap;
      end if;

      if not Have_Heap then
         return System.Null_Address;
      end if;

      declare
         Need : constant Storage_Count :=
           Header_Size
             + (Size + Heap_Alignment - 1) / Heap_Alignment * Heap_Alignment;
      begin
         for Attempt in 1 .. 2 loop
            declare
               Block : System.Address := Heap_Base;
            begin
               while Block < Heap_Top loop
                  declare
                     This : constant Block_Header := Header (Block);
                     Block_Size : Storage_Count := This.Size;
                  begin
                     if This.Free then
                        Block_Size := Merge_Free (Block);
                        if Block_Size >= Need then
                           if Block_Size - Need >= Min_Block_Size then
                              Set_Header
                                (Block + Need, Block_Size - Need, True);
                              Block_Size := Need;
                           end if;
                           Set_Header (Block, Block_Size, False);
                           return Block + Header_Size;
                        end if;
                     end if;
                     Block := Block + Block_Size;
                  end;
               end loop;
            end;

            exit when Attempt = 2 or else not Grow_Heap (Need);
         end loop;
      end;

      return System.Null_Address;
   end Allocate;

   -------------------
   -- Current_Bound --
   -------------------

   function Current_Bound return System.Address is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Current_Bound_Cap);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         return System.Null_Address;
      end if;
      return To_Address
        (Integer_Address (Rose.System_Calls.Get_Word_32 (Params, 0))
         * Rose.Limits.Page_Size);
   end Current_Bound;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Ptr : System.Address)
   is
   begin
      if Ptr /= System.Null_Address then
         Set_Header (Block_Of (Ptr), Header (Block_Of (Ptr)).Size, True);
      end if;
   end Deallocate;

   ---------------
   -- Grow_Heap --
   ---------------

   function Grow_Heap (Size : Storage_Count) return Boolean is
      Last  : System.Address := System.Null_Address;
      Block : System.Address := Heap_Base;
      Start : System.Address;
   begin
      while Block < Heap_Top loop
         Last := Block;
         Block := Block + Header (Block).Size;
      end loop;

      if Last /= System.Null_Address and then Header (Last).Free then
         Start := Last;
      else
         Start := Heap_Top;
      end if;

      declare
         Page_Size : constant Integer_Address := Rose.Limits.Page_Size;
         Want      : constant Integer_Address :=
           (To_Integer (Start) + Integer_Address (Size) + Page_Size - 1)
           / Page_Size;
         Params    : aliased Rose.Invocation.Invocation_Record;
         New_Top   : System.Address;
      begin
         Rose.System_Calls.Initialize_Send (Params, Request_Bound_Cap);
         Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Want));
         Rose.System_Calls.Invoke_Capability (Params);

         New_Top := Current_Bound;
         if New_Top = System.Null_Address
           or else New_Top < Start + Size
         then
            return False;
         end if;

         Set_Header (Start, New_Top - Start, True);
         Heap_Top := New_Top;
         return True;
      end;
   end Grow_Heap;

   ------------
   -- Header --
   ------------

   function Header (Block : System.Address) return Block_Header is
      Item : constant Block_Header
        with Import, Address => Block;
   begin
      return Item;
   end Header;

   ----------------
   -- Merge_Free --
   ----------------

   function Merge_Free (Block : System.Address) return Storage_Count is
      Size : Storage_Count := Header (Block).Size;
   begin
      while Block + Size < Heap_Top
        and then Header (Block + Size).Free
      loop
         Size := Size + Header (Block + Size).Size;
      end loop;

      Set_Header (Block, Size, True);
      return Size;
   end Merge_Free;

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header
     (Block : System.Address;
      Size  : Storage_Count;
      Free  : Boolean)
   is
      Item : Block_Header
        with Import, Address => Block;
   begin
      Item := (Size => Size, Free => Free);
   end Set_Header;

   ----------------
   -- Start_Heap --
   ----------------

   procedure Start_Heap is
      Params   : aliased Rose.Invocation.Invocation_Record;
      Heap_Cap : Rose.Capabilities.Capability;
   begin
      Heap_Started := True;

      Rose.System_Calls.Initialize_Send
        (Params, Rose.Capabilities.Standard.Process_Interface_Cap);
      Rose.System_Calls.Receive_Caps
        (Params, Rose.Interfaces.Process.Subprogram_Count);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         return;
      end if;

      Rose.System_Calls.Initialize_Send
        (Params, Params.Caps (Rose.Interfaces.Process.Heap_Interface_Index));
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         return;
      end if;
      Heap_Cap := Params.Caps (0);

      Rose.System_Calls.Initialize_Send (Params, Heap_Cap);
      Rose.System_Calls.Receive_Caps
        (Params, Rose.Interfaces.Heap.Subprogram_Count);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         return;
      end if;
      Current_Bound_Cap :=
        Params.Caps (Rose.Interfaces.Heap.Current_Bound_Index);
      Request_Bound_Cap :=
        Params.Caps (Rose.Interfaces.Heap.Request_New_Bound_Index);

      Heap_Base := Current_Bound;
      Heap_Top := Heap_Base;
      Have_Heap := Heap_Base /= System.Null_Address;
   end Start_Heap;

   procedure Put_Char_Stderr
     (Ch : Character);

   pragma Export (C, Put_Char_Stderr, "put_char_stderr");

   procedure Unwind_Resume is null;

   pragma Export (C, Unwind_Resume, "_Unwind_Resume");

   procedure Put_Char_Stderr
     (Ch : Character)
   is null;

   function Reallocate
     (Ptr  : System.Address;
      Size : System.Storage_Elements.Storage_Count)
     return System.Address
   is
      Capacity : Storage_Count;
      Result   : System.Address;
   begin
      if Ptr = System.Null_Address then
         return Allocate (Size);
      end if;

      Capacity := Header (Block_Of (Ptr)).Size - Header_Size;
      if Size <= Capacity then
         return Ptr;
      end if;

      Result := Allocate (Size);
      if Result /= System.Null_Address then
         declare
            Source : constant Storage_Array (1 .. Capacity)
              with Import, Address => Ptr;
            Target : Storage_Array (1 .. Capacity)
              with Import, Address => Result;
         begin
            Target := Source;
         end;
         Deallocate (Ptr);
      end if;

      return Result;
   end Reallocate;

   procedure Gnat_Personality_V0 is null;

   pragma Export (C, Gnat_Personality_V0, "__gnat_personality_v0");

   procedure Unhandled_Terminate;
--  pragma No_Return (Unhandled_Terminate);
   pragma Export (C, Unhandled_Terminate, "__gnat_unhandled_terminate");

   Gnat_Argv : System.Address;
   pragma Export (C, Gnat_Argv, "gnat_argv");

   procedure Fill_Arg (A : System.Address; Arg_Num : Integer);
   pragma Export (C, Fill_Arg, "__gnat_fill_arg");

   function Len_Arg (Arg_Num : Integer) return Integer;
   pragma Export (C, Len_Arg, "__gnat_len_arg");

   procedure Unhandled_Terminate is null;

   function Backtrace
     (Traceback   : System.Address;
      Len         : Integer;
      Exclude_Min : System.Address;
      Exclude_Max : System.Address;
      Skip_Frames : Integer)
     return        Integer
   is
      pragma Unreferenced
        (Traceback, Len, Exclude_Min, Exclude_Max, Skip_Frames);
   begin
      return 0;
   end Backtrace;

   function CleanupUnwind_Handler
     (UW_Version   : Integer;
      UW_Phases    : Unwind_Action;
      UW_Eclass    : Exception_Class;
      UW_Exception : not null GCC_Exception_Access;
      UW_Context   : System.Address;
      UW_Argument  : System.Address) return Unwind_Reason_Code
   is
      pragma Unreferenced
        (UW_Version, UW_Phases, UW_Eclass,
         UW_Exception, UW_Context, UW_Argument);
   begin
      return 0;
   end CleanupUnwind_Handler;

   procedure Fill_Arg (A : System.Address; Arg_Num : Integer)
   is null;

   function Len_Arg (Arg_Num : Integer) return Integer is
      pragma Unreferenced (Arg_Num);
   begin
      return 0;
   end Len_Arg;

end Rose.CRTL;
