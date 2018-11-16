--  Support for Kernel modules
with System.Storage_Elements;

package Rose.Kernel.Modules is

   Max_Kernel_Modules : constant := 16;

   type Module_Index is range 1 .. Max_Kernel_Modules;

   Init_Module              : constant Module_Index := 1;
   Console_Module           : constant Module_Index := 2;
   Memory_Management_Module : constant Module_Index := 3;

   function Last_Boot_Module return Module_Index;

   procedure Relocate is null;
   --  Move modules to safe memory

   procedure Add_Module
     (Base, Bound : Physical_Address);

   procedure Get_Module_Image
     (Module : Module_Index;
      Base   : out System.Address;
      Length : out System.Storage_Elements.Storage_Count);

end Rose.Kernel.Modules;
