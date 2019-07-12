with Rose.Addresses;

package body Rose.CRTL is

   type GCC_Exception_Access is access all Integer;

   type Unwind_Action is new Integer;

   type Exception_Class is new Integer;

   type Unwind_Reason_Code is new Integer;

   Current_Top : Rose.Addresses.Virtual_Page_Address := 0
     with Unreferenced;

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

   function Allocate
     (Size : System.Storage_Elements.Storage_Count)
     return System.Address
   is
      pragma Unreferenced (Size);
   begin
      return System.Null_Address;
   end Allocate;

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
      pragma Unreferenced (Ptr);
   begin
      return Allocate (Size);
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
