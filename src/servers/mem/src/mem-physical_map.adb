with Rose.Limits;
with Rose.Words;

package body Mem.Physical_Map is

   use Rose.Addresses;

   type Allocated_Block is new Rose.Words.Word_64;

   Allocated_Block_Count : constant :=
                             Rose.Limits.Max_Physical_Pages
                               / Allocated_Block'Size;

   type Allocated_Block_Array is
     array (Physical_Page_Address range 0 .. Allocated_Block_Count - 1)
     of Allocated_Block;

   Allocation_Map : Allocated_Block_Array := (others => 0);

   type Physical_Region is
      record
         Base, Bound    : Physical_Page_Address;
         Map_Base       : Physical_Page_Address;
         Map_Bound      : Physical_Page_Address;
         Map_Page_Cap   : Rose.Capabilities.Capability;
         Unmap_Page_Cap : Rose.Capabilities.Capability;
      end record;

   Max_Regions : constant := 4;
   type Region_Count is range 0 .. Max_Regions;
   subtype Region_Index is Region_Count range 1 .. Region_Count'Last;

   Region_Table      : array (Region_Index) of Physical_Region;
   Number_Of_Regions : Region_Count := 0;

   ----------------
   -- Add_Region --
   ----------------

   procedure Add_Region
     (Base, Bound    : Rose.Addresses.Physical_Page_Address;
      Map_Page_Cap   : Rose.Capabilities.Capability;
      Unmap_Page_Cap : Rose.Capabilities.Capability)
   is
      Map_Start : constant Physical_Page_Address :=
                    (if Number_Of_Regions = 0
                     then 0
                     else Region_Table (Number_Of_Regions).Map_Bound + 1);
   begin
      Number_Of_Regions := Number_Of_Regions + 1;
      Region_Table (Number_Of_Regions) :=
        (Base, Bound, Map_Start,
         Map_Start + (Bound - Base) / Allocated_Block'Size,
         Map_Page_Cap, Unmap_Page_Cap);
      if Bound mod Allocated_Block'Size /= 0 then
         Allocation_Map (Region_Table (Number_Of_Regions).Bound - 1) :=
           Allocated_Block'Last
             - (2 ** Natural (Bound mod Allocated_Block'Size) - 1);
      end if;

   end Add_Region;

   -------------------
   -- Allocate_Page --
   -------------------

   procedure Allocate_Page
     (Page    : out Rose.Addresses.Physical_Page_Address;
      Success : out Boolean)
   is
      Region : Region_Index := Region_Index'First;

   begin
      for I in Allocation_Map'Range loop
         if Region_Table (Region).Map_Bound <= I then
            if Region = Number_Of_Regions then
               Success := False;
               return;
            end if;
            Region := Region + 1;
         end if;

         if Allocation_Map (I) /= Allocated_Block'Last then
            Page := Region_Table (Region).Base
              + Allocated_Block'Size * (I - Region_Table (Region).Map_Base);
            declare
               It : Allocated_Block := Allocation_Map (I);
               Offset : Natural := 0;
            begin
               while It mod 2 = 1 loop
                  It := It / 2;
                  Offset := Offset + 1;
               end loop;
               Allocation_Map (I) := Allocation_Map (I)
                 or 2 ** Offset;
               Page := Page + Physical_Page_Address (Offset);
               Success := True;
               return;
            end;
         end if;
      end loop;
      Success := False;
   end Allocate_Page;

   ---------------------
   -- Deallocate_Page --
   ---------------------

   procedure Deallocate_Page (Page : Rose.Addresses.Physical_Page_Address) is
   begin
      for I in 1 .. Number_Of_Regions loop
         if Page in Region_Table (I).Base .. Region_Table (I).Bound - 1 then
            declare
               M : Allocated_Block renames
                     Allocation_Map (Region_Table (I).Map_Base
                                     + (Page - Region_Table (I).Base)
                                     / Allocated_Block'Size);
            begin
               M := M and not
                 (2 ** Natural ((Page - Region_Table (I).Base)
                                mod Allocated_Block'Size));
            end;
            return;
         end if;
      end loop;
   end Deallocate_Page;

   ----------------
   -- Region_Cap --
   ----------------

   function Region_Map_Page_Cap
     (Page : Rose.Addresses.Physical_Page_Address)
      return Rose.Capabilities.Capability
   is
   begin
      for Region of Region_Table loop
         if Page in Region.Base .. Region.Bound - 1 then
            return Region.Map_Page_Cap;
         end if;
      end loop;
      return 0;
   end Region_Map_Page_Cap;

   ---------------------------
   -- Region_Unmap_Page_Cap --
   ---------------------------

   function Region_Unmap_Page_Cap
      return Rose.Capabilities.Capability
   is
   begin
      return Region_Table (1).Unmap_Page_Cap;
   end Region_Unmap_Page_Cap;

end Mem.Physical_Map;
