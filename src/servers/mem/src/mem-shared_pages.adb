with Rose.Console_IO;

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
         Unsaved  : Boolean := False;
         Page     : Rose.Objects.Object_Id := 0;
         Physical : Rose.Addresses.Physical_Page_Address := 0;
         Mappings : Natural := 0;
      end record;
   --  Modified is True if the page has been written since it was read
   --  from its region, and Unsaved if it has been written since the last
   --  checkpoint.

   Table      : array (1 .. Max_Shared_Pages) of Shared_Page;
   Used_Count : Natural := 0;

   Released       : array (1 .. Max_Shared_Pages) of Rose.Objects.Object_Id;
   Released_Count : Natural := 0;
   --  The pages released since the last checkpoint.

   procedure Add_Released (Page : Rose.Objects.Object_Id);

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
                     Unsaved  => False,
                     Page     => Page,
                     Physical => Physical,
                     Mappings => 1);
            Used_Count := Used_Count + 1;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Add;

   ------------------
   -- Add_Released --
   ------------------

   procedure Add_Released (Page : Rose.Objects.Object_Id) is
   begin
      for Item of Released (1 .. Released_Count) loop
         if Item = Page then
            return;
         end if;
      end loop;

      if Released_Count = Released'Last then
         Rose.Console_IO.Put_Line
           ("mem: too many released shared pages for one checkpoint");
         return;
      end if;

      Released_Count := Released_Count + 1;
      Released (Released_Count) := Page;
   end Add_Released;

   ----------------------------
   -- Clear_Checkpoint_Pages --
   ----------------------------

   procedure Clear_Checkpoint_Pages is
   begin
      for Item of Table loop
         Item.Unsaved := False;
      end loop;
      Released_Count := 0;
   end Clear_Checkpoint_Pages;

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

   --------------------------
   -- Has_Checkpoint_Pages --
   --------------------------

   function Has_Checkpoint_Pages return Boolean is
   begin
      return Released_Count > 0
        or else (for some Item of Table => Item.Used and then Item.Unsaved);
   end Has_Checkpoint_Pages;

   --------------
   -- Index_Of --
   --------------

   function Index_Of
     (Physical : Rose.Addresses.Physical_Page_Address)
      return Natural
   is
   begin
      --  Most of the time nothing is shared, and a checkpoint asks about
      --  every dirty page.

      if Used_Count = 0 then
         return 0;
      end if;

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

   ----------------------
   -- Iterate_Released --
   ----------------------

   procedure Iterate_Released
     (Action : not null access
        procedure (Page : Rose.Objects.Page_Object_Id))
   is
   begin
      for Page of Released (1 .. Released_Count) loop
         Action (Page);
      end loop;
   end Iterate_Released;

   ---------------------
   -- Iterate_Unsaved --
   ---------------------

   procedure Iterate_Unsaved
     (Action : not null access
        procedure (Page     : Rose.Objects.Page_Object_Id;
                   Physical : Rose.Addresses.Physical_Page_Address))
   is
   begin
      for Item of Table loop
         if Item.Used and then Item.Unsaved then
            Action (Item.Page, Item.Physical);
         end if;
      end loop;
   end Iterate_Unsaved;

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
            Add_Released (Item.Page);
            Item := (others => <>);
            Used_Count := Used_Count - 1;
         end if;
      end;
   end Release;

   -------------
   -- Restore --
   -------------

   procedure Restore
     (Page     : Rose.Objects.Page_Object_Id;
      Physical : Rose.Addresses.Physical_Page_Address;
      Success  : out Boolean)
   is
   begin
      Success := False;

      if Find (Page) /= 0 then
         return;
      end if;

      for Item of Table loop
         if not Item.Used then
            Item := (Used     => True,
                     Modified => True,
                     Unsaved  => True,
                     Page     => Page,
                     Physical => Physical,
                     Mappings => 0);
            Used_Count := Used_Count + 1;
            Success := True;
            return;
         end if;
      end loop;
   end Restore;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified (Physical : Rose.Addresses.Physical_Page_Address) is
      Index : constant Natural := Index_Of (Physical);
   begin
      if Index /= 0 then
         Table (Index).Modified := True;
         Table (Index).Unsaved := True;
      end if;
   end Set_Modified;

end Mem.Shared_Pages;
