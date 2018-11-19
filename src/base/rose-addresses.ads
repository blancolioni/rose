with System;

with Ada.Unchecked_Conversion;

with Rose.Objects;
with Rose.Words;

package Rose.Addresses is

   type Physical_Page_Address is mod 2 ** 20;
   Physical_Page_Address_Bits : constant := 20;

   Physical_Page_Bytes : constant := 4096;
   Physical_Page_Bits  : constant := 8 * Physical_Page_Bytes;

   type Physical_Address is new Rose.Words.Word;
   subtype Physical_Bytes is Physical_Address;

   type Virtual_Page_Address  is new Physical_Page_Address;

   Virtual_Page_Address_Bits  : constant := Physical_Page_Address_Bits;

   Virtual_Page_Bytes : constant := Physical_Page_Bytes;
   Virtual_Page_Bits  : constant := 8 * Virtual_Page_Bytes;

   Memory_Page_Bits   : constant := 12;
   Memory_Page_Size   : constant := 2**Memory_Page_Bits;
   type Memory_Page_Offset is mod Memory_Page_Size;

   type Virtual_Address is new Rose.Words.Word_32;
   subtype Virtual_Bytes is Virtual_Address;

   type Kernel_Address is
     new Virtual_Address range 16#F000_0000# .. 16#FFFF_FFFF#;

   type Kernel_Page_Address is
     new Virtual_Page_Address range
       Virtual_Page_Address (Kernel_Address'First / Virtual_Page_Bytes)
         .. Virtual_Page_Address (Kernel_Address'Last / Virtual_Page_Bytes);

   function Page_Object_Id_To_Physical_Page_Address
     (Object_Id : Rose.Objects.Page_Object_Id)
      return Physical_Page_Address;

   function Page_Object_Id_To_Virtual_Page_Address
     (Object_Id : Rose.Objects.Page_Object_Id)
      return Virtual_Page_Address;

   --  converting between pages and addresses
   function Physical_Page_To_Address
     (Item : Physical_Page_Address)
     return Physical_Address;

   function Virtual_Page_To_Address
     (Item : Virtual_Page_Address)
     return Virtual_Address;

   function Physical_Address_To_Page
     (Item : Physical_Address)
     return Physical_Page_Address;

   function Virtual_Address_To_Page
     (Item : Virtual_Address)
      return Virtual_Page_Address
   is (Virtual_Page_Address (Item / Virtual_Page_Bytes));

   function Align_Up_To_Unit
     (Addr : Physical_Address;
      Unit : Physical_Address)
     return Physical_Address;

   function Align_Down_To_Unit
     (Addr : Physical_Address;
      Unit : Physical_Address)
     return Physical_Address;

   function Align_Up_To_Unit
     (Addr : Virtual_Address;
      Unit : Virtual_Address)
     return Virtual_Address;

   function Align_Down_To_Unit
     (Addr : Virtual_Address;
      Unit : Virtual_Address)
     return Virtual_Address;

   function Align_Up_To_Page_Boundary
     (Addr : Virtual_Address)
     return Virtual_Address;

   function Align_Up_To_Page_Boundary
     (Addr : Physical_Address)
     return Physical_Address;

   --  converting between physical/virtual addresses and system.address
   function To_System_Address is
     new Ada.Unchecked_Conversion (Physical_Address, System.Address);

   function To_System_Address is
     new Ada.Unchecked_Conversion (Virtual_Address, System.Address);

   function To_Virtual_Address is
      new Ada.Unchecked_Conversion (System.Address, Virtual_Address);

private

   function Align_Up_To_Unit
     (Addr : Physical_Address;
      Unit : Physical_Address)
     return Physical_Address
   is (if Addr mod Unit = 0
         then Addr
         else Addr + Unit - Addr mod Unit);

   function Align_Down_To_Unit
     (Addr : Physical_Address;
      Unit : Physical_Address)
     return Physical_Address
   is (Addr - Addr mod Unit);

   function Align_Up_To_Page_Boundary
     (Addr : Virtual_Address)
     return Virtual_Address
   is (Align_Up_To_Unit (Addr, Virtual_Page_Bytes));

   function Align_Up_To_Page_Boundary
     (Addr : Physical_Address)
     return Physical_Address
   is (Align_Up_To_Unit (Addr, Physical_Page_Bytes));

   function Page_Object_Id_To_Physical_Page_Address
     (Object_Id : Rose.Objects.Page_Object_Id)
      return Physical_Page_Address
   is (Physical_Page_Address (Rose.Objects.To_Page_Id (Object_Id)));

   function Page_Object_Id_To_Virtual_Page_Address
     (Object_Id : Rose.Objects.Page_Object_Id)
      return Virtual_Page_Address
   is (Virtual_Page_Address (Rose.Objects.To_Page_Id (Object_Id)));

end Rose.Addresses;
