with Rose.Addresses;

package body Rose.CRTL is

   Current_Top : Rose.Addresses.Virtual_Page_Address := 0
     with Unreferenced;

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
     (Ch : Character)
   is null;

   procedure Unwind_Resume is null;

end Rose.CRTL;
