package body Mem.Page_Cache is

   use type Rose.Addresses.Physical_Page_Address;
   use type Rose.Objects.Object_Id;

   Max_Cached_Pages : constant := 512;

   type Cached_Page is
      record
         Used     : Boolean := False;
         Page     : Rose.Objects.Page_Object_Id;
         Physical : Rose.Addresses.Physical_Page_Address := 0;
         Mappings : Natural := 0;
      end record;

   Cache : array (1 .. Max_Cached_Pages) of Cached_Page;

   function Index_Of
     (Physical : Rose.Addresses.Physical_Page_Address)
      return Natural;
   --  The entry that caches Physical, or zero.

   -------------
   -- Acquire --
   -------------

   procedure Acquire (Physical : Rose.Addresses.Physical_Page_Address) is
      Index : constant Natural := Index_Of (Physical);
   begin
      if Index /= 0 then
         Cache (Index).Mappings := Cache (Index).Mappings + 1;
      end if;
   end Acquire;

   ---------
   -- Add --
   ---------

   procedure Add
     (Page     : Rose.Objects.Page_Object_Id;
      Physical : Rose.Addresses.Physical_Page_Address)
   is
   begin
      for Item of Cache loop
         if not Item.Used then
            Item := (Used     => True,
                     Page     => Page,
                     Physical => Physical,
                     Mappings => 1);
            return;
         end if;
      end loop;
   end Add;

   -----------
   -- Evict --
   -----------

   procedure Evict
     (Physical : out Rose.Addresses.Physical_Page_Address;
      Success  : out Boolean)
   is
   begin
      for Item of Cache loop
         if Item.Used and then Item.Mappings = 0 then
            Physical := Item.Physical;
            Item.Used := False;
            Success := True;
            return;
         end if;
      end loop;
      Physical := 0;
      Success := False;
   end Evict;

   ----------
   -- Find --
   ----------

   function Find
     (Page : Rose.Objects.Page_Object_Id)
      return Rose.Addresses.Physical_Page_Address
   is
   begin
      for Item of Cache loop
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
      for Index in Cache'Range loop
         if Cache (Index).Used and then Cache (Index).Physical = Physical then
            return Index;
         end if;
      end loop;
      return 0;
   end Index_Of;

   -------------
   -- Release --
   -------------

   procedure Release
     (Physical : Rose.Addresses.Physical_Page_Address;
      Cached   : out Boolean)
   is
      Index : constant Natural := Index_Of (Physical);
   begin
      Cached := Index /= 0;
      if Cached and then Cache (Index).Mappings > 0 then
         Cache (Index).Mappings := Cache (Index).Mappings - 1;
      end if;
   end Release;

end Mem.Page_Cache;
