package body Rose.Kernel.Modules is

   subtype Module_Name is String (1 .. 16);

   type Module_Info is
      record
         Name        : Module_Name;
         Base, Bound : Virtual_Address;
      end record;

   type Module_Array is array (Module_Index) of Module_Info;

   Loaded_Modules : Module_Array := (others => ((others => ' '), 0, 0));
   Module_Count   : Natural := 0;

   ----------------
   -- Add_Module --
   ----------------

   procedure Add_Module
     (Name        : String;
      Base, Bound : Physical_Address)
   is
      Mod_Name : Module_Name := (others => ' ');
   begin
      if Name'Length <= 16 then
         Mod_Name (1 .. Name'Length) := Name;
      else
         Mod_Name := Name (Name'First .. Name'First + 15);
      end if;

      Module_Count := Module_Count + 1;
      Loaded_Modules (Module_Index (Module_Count)) :=
        (Mod_Name,
         Virtual_Address (Base) + Kernel_Virtual_Base,
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

   ---------------------
   -- Get_Module_Name --
   ---------------------

   procedure Get_Module_Name
     (Module  : Module_Index;
      Name    : out String;
      Last    : out Natural)
   is
   begin
      Last := Name'First - 1;
      for I in Loaded_Modules (Module).Name'Range loop
         declare
            Ch : constant Character := Loaded_Modules (Module).Name (I);
         begin
            exit when Ch = ' ';
            Last := Last + 1;
            Name (Last) := Ch;
         end;
      end loop;
   end Get_Module_Name;

   ----------------------
   -- Last_Boot_Module --
   ----------------------

   function Last_Boot_Module return Module_Index is
   begin
      return Module_Index (Module_Count);
   end Last_Boot_Module;

end Rose.Kernel.Modules;
