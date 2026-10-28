package body Rose.CRTL is

   use System.Storage_Elements;

   type GCC_Exception_Access is access all Integer;

   type Unwind_Action is new Integer;

   type Exception_Class is new Integer;

   type Unwind_Reason_Code is new Integer;

   Heap_Alignment : constant := Standard'Maximum_Alignment;

   Header_Size : constant Storage_Count := Heap_Alignment;
   --  Each block starts with a header that holds its size, which
   --  Reallocate needs. Keeping the header a whole alignment unit keeps
   --  the block that follows aligned.

   Heap        : Storage_Array (1 .. 65536)
     with Alignment => Heap_Alignment;
   Current_Top : Storage_Offset := Heap'First;
   --  A bump allocator: Deallocate does nothing, so freed memory is not
   --  reused. A real heap waits for space banks.

   function Block_Size (Ptr : System.Address) return Storage_Count;
   --  The size that the block at Ptr was allocated with.

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
   begin
      if Size = 0 or else Size > Heap'Length then
         return System.Null_Address;
      end if;

      declare
         Rounded : constant Storage_Count :=
           (Size + Heap_Alignment - 1) / Heap_Alignment * Heap_Alignment;
      begin
         if Header_Size + Rounded > Heap'Last - Current_Top + 1 then
            return System.Null_Address;
         end if;

         declare
            Header : Storage_Count
              with Import, Address => Heap (Current_Top)'Address;
         begin
            Header := Size;
         end;

         return Result : constant System.Address :=
           Heap (Current_Top + Header_Size)'Address
         do
            Current_Top := Current_Top + Header_Size + Rounded;
         end return;
      end;
   end Allocate;

   ----------------
   -- Block_Size --
   ----------------

   function Block_Size (Ptr : System.Address) return Storage_Count is
      Header : constant Storage_Count
        with Import, Address => Ptr - Header_Size;
   begin
      return Header;
   end Block_Size;

   procedure Deallocate
     (Ptr : System.Address)
   is null;

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
      use type System.Address;
      Result : constant System.Address := Allocate (Size);
   begin
      if Ptr /= System.Null_Address
        and then Result /= System.Null_Address
      then
         declare
            Count  : constant Storage_Count :=
              Storage_Count'Min (Block_Size (Ptr), Size);
            Source : constant Storage_Array (1 .. Count)
              with Import, Address => Ptr;
            Target : Storage_Array (1 .. Count)
              with Import, Address => Result;
         begin
            Target := Source;
         end;
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
