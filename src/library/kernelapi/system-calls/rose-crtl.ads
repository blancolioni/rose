with System.Storage_Elements;

package Rose.CRTL is

   function Allocate
     (Size : System.Storage_Elements.Storage_Count)
     return System.Address;

   pragma Export (C, Allocate, "malloc");

   procedure Deallocate
     (Ptr : System.Address);

   pragma Export (C, Deallocate, "free");

   procedure Unwind_Resume;

   pragma Export (C, Unwind_Resume, "_Unwind_Resume");

   procedure Put_Char_Stderr
     (Ch : Character);

   pragma Export (C, Put_Char_Stderr, "put_char_stderr");

end Rose.CRTL;
