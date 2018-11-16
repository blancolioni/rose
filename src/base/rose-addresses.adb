package body Rose.Addresses is

   ------------------------
   -- Align_Down_To_Unit --
   ------------------------

   function Align_Down_To_Unit (Addr : Virtual_Address;
                                Unit : Virtual_Address)
                               return Virtual_Address
   is
      Result : Virtual_Address := Addr;
   begin
      if Result mod Unit /= 0 then
         Result := Result - Result mod Unit;
      end if;
      return Result;
   end Align_Down_To_Unit;

   ----------------------
   -- Align_Up_To_Unit --
   ----------------------

   function Align_Up_To_Unit
     (Addr : Virtual_Address;
      Unit : Virtual_Address)
     return Virtual_Address
   is
      Result : Virtual_Address := Addr;
   begin
      if Result mod Unit /= 0 then
         Result := Result + Unit - (Result mod Unit);
      end if;
      return Result;
   end Align_Up_To_Unit;

   ------------------------------
   -- Physical_Address_To_Page --
   ------------------------------

   function Physical_Address_To_Page (Item : Physical_Address)
                                     return Physical_Page_Address
   is
   begin
      return Physical_Page_Address (Item / Physical_Page_Bytes);
   end Physical_Address_To_Page;

   ------------------------------
   -- Physical_Page_To_Address --
   ------------------------------

   function Physical_Page_To_Address (Item : Physical_Page_Address)
                                     return Physical_Address
   is
   begin
      return Physical_Address (Item) * Physical_Page_Bytes;
   end Physical_Page_To_Address;

   -----------------------------
   -- Virtual_Page_To_Address --
   -----------------------------

   function Virtual_Page_To_Address (Item : Virtual_Page_Address)
                                     return Virtual_Address
   is
   begin
      return Virtual_Address (Item) * Virtual_Page_Bytes;
   end Virtual_Page_To_Address;

end Rose.Addresses;
