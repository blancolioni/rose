package body Rose.Kernel.Modules is

   type Module_Info is
      record
         Base, Bound : Virtual_Address;
      end record;

   type Module_Array is array (Module_Index) of Module_Info;

   Loaded_Modules : Module_Array := (others => (0, 0));
   Module_Count   : Natural := 0;

   ----------------
   -- Add_Module --
   ----------------

   procedure Add_Module
     (Base, Bound : Physical_Address)
   is
   begin
      Module_Count := Module_Count + 1;
      Loaded_Modules (Module_Index (Module_Count)) :=
        (Virtual_Address (Base) + Kernel_Virtual_Base,
         Virtual_Address (Bound) + Kernel_Virtual_Base);
   end Add_Module;

   ----------------------
   -- Get_Module_Image --
   ----------------------

   procedure Get_Module_Image
     (Module : Module_Index;
      Base   : out System.Address;
      Length : out System.Storage_Elements.Storage_Count)
   is
      Info : Module_Info renames
               Loaded_Modules (Module);
   begin
      Base := Rose.Addresses.To_System_Address (Info.Base);
      Length :=
        System.Storage_Elements.Storage_Count
          (Info.Bound - Info.Base);
   end Get_Module_Image;

   ----------------------
   -- Last_Boot_Module --
   ----------------------

   function Last_Boot_Module return Module_Index is
   begin
      return Module_Index (Module_Count);
   end Last_Boot_Module;

end Rose.Kernel.Modules;
