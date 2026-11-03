package body Mem.Shared_Pages is

   use type Rose.Addresses.Physical_Page_Address;
   use type Rose.Objects.Object_Id;

   Max_Shared_Pages : constant := 2048;
   --  Room for two full-screen 1024 x 768 buffers of 32-bit pixels, and
   --  more besides.

   type Shared_Page is
      record
         Used     : Boolean := False;
         Modified : Boolean := False;
         Page     : Rose.Objects.Object_Id := 0;
         Physical : Rose.Addresses.Physical_Page_Address := 0;
         Mappings : Natural := 0;
      end record;

   Table : array (1 .. Max_Shared_Pages) of Shared_Page;

   function Index_Of
     (Physical : Rose.Addresses.Physical_Page_Address)
      return Natural;
   --  The entry for Physical, or zero.

   -------------
   -- Acquire --
   -------------

   procedure Acquire (Physical : Rose.Addresses.Physical_Page_Address) is
      Index : constant Natural := Index_Of (Physical);
   begin
      if Index /= 0 then
         Table (Index).Mappings := Table (Index).Mappings + 1;
      end if;
   end Acquire;

   ---------
   -- Add --
   ---------

   procedure Add
     (Page     : Rose.Objects.Page_Object_Id;
      Physical : Rose.Addresses.Physical_Page_Address;
      Success  : out Boolean)
   is
   begin
      for Item of Table loop
         if not Item.Used then
            Item := (Used     => True,
                     Modified => False,
                     Page     => Page,
                     Physical => Physical,
                     Mappings => 1);
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Add;

   ----------
   -- Find --
   ----------

   function Find
     (Page : Rose.Objects.Page_Object_Id)
      return Rose.Addresses.Physical_Page_Address
   is
   begin
      for Item of Table loop
         if Item.Used and then Item.Page = Page then
            return Item.Physical;
         end if;
      end loop;
      return 0;
   end Find;

   --------------
   -- Index_Of --
   --------------

   function Index_Of
     (Physical : Rose.Addresses.Physical_Page_Address)
      return Natural
   is
   begin
      for Index in Table'Range loop
         if Table (Index).Used and then Table (Index).Physical = Physical then
            return Index;
         end if;
      end loop;
      return 0;
   end Index_Of;

   ---------------
   -- Is_Shared --
   ---------------

   function Is_Shared
     (Physical : Rose.Addresses.Physical_Page_Address)
      return Boolean
   is
   begin
      return Index_Of (Physical) /= 0;
   end Is_Shared;

   -------------
   -- Release --
   -------------

   procedure Release
     (Physical : Rose.Addresses.Physical_Page_Address;
      Last     : out Boolean;
      Modified : out Boolean)
   is
      Index : constant Natural := Index_Of (Physical);
   begin
      Last := False;
      Modified := False;

      if Index = 0 then
         return;
      end if;

      declare
         Item : Shared_Page renames Table (Index);
      begin
         if Item.Mappings > 1 then
            Item.Mappings := Item.Mappings - 1;
         else
            Last := True;
            Modified := Item.Modified;
            Item := (others => <>);
         end if;
      end;
   end Release;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified (Physical : Rose.Addresses.Physical_Page_Address) is
      Index : constant Natural := Index_Of (Physical);
   begin
      if Index /= 0 then
         Table (Index).Modified := True;
      end if;
   end Set_Modified;

end Mem.Shared_Pages;
