package body Rose.Kernel.Cap_Tables is

   use type System.Address;
   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word_32;

   function Load
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Cached_Capability_Index;
   --  The entry of the cache that holds the layout of Cap, which is put
   --  there, in the place of the one that was used longest ago, if it is
   --  not.  The page that holds Cap must have been set, and so must the
   --  page of the capability that is replaced, which it was.

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability)
   is
   begin
      if Cap in 1 .. Max_Capability_Index then
         Table.Is_Active (Cap) := True;
      end if;
   end Activate;

   ------------
   -- Create --
   ------------

   procedure Create
     (Table : in out Cap_Table;
      Cap   : out Rose.Capabilities.Capability)
   is
   begin
      for Number in Rose.Capabilities.Capability range
        1 .. Max_Capability_Index
      loop
         if not Table.Is_Active (Number) then
            Table.Is_Active (Number) := True;
            if Number > Table.Last_Cap then
               Table.Last_Cap := Number;
            end if;
            Cap := Number;
            return;
         end if;
      end loop;

      Cap := 0;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability)
   is
   begin
      Set (Table, Cap, (others => <>));

      if Cap in 1 .. Max_Capability_Index then
         Table.Is_Active (Cap) := False;
      end if;
   end Delete;

   ---------
   -- Get --
   ---------

   procedure Get
     (Table  : in out Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Layout : out Rose.Capabilities.Layout.Capability_Layout)
   is
      Index : constant Cached_Capability_Index := Load (Table, Cap);
   begin
      Layout := Table.Cached_Caps (Index).Layout;
   end Get;

   --------------
   -- Has_Page --
   --------------

   function Has_Page
     (Table : Cap_Table;
      Page  : Capability_Page_Index)
      return Boolean
   is (Table.Cap_Pages (Page) /= System.Null_Address);

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Boolean
   is (Cap in 1 .. Max_Capability_Index
       and then Cap <= Table.Last_Cap
       and then Table.Is_Active (Cap));

   --------------
   -- Last_Cap --
   --------------

   function Last_Cap
     (Table : Cap_Table)
      return Rose.Capabilities.Capability
   is (Table.Last_Cap);

   ----------
   -- Load --
   ----------

   function Load
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Cached_Capability_Index
   is
      Result : Cached_Capability_Count := 0;
   begin
      if Cap in 1 .. Max_Capability_Index and then Table.Is_Cached (Cap) then
         for I in Table.Cached_Caps'Range loop
            if Table.Cached_Caps (I).Cap = Cap then
               Result := I;
               exit;
            end if;
         end loop;
      end if;

      if Result = 0 then
         declare
            Min_Tick : Rose.Words.Word_32 := Rose.Words.Word_32'Last;
            Save_Cap : Boolean := True;
         begin
            for I in Table.Cached_Caps'Range loop
               declare
                  Cached : Cached_Capability renames Table.Cached_Caps (I);
               begin
                  if Cached.Cap = 0 then
                     Result := I;
                     Save_Cap := False;
                     exit;
                  end if;
                  if Cached.Tick < Min_Tick then
                     Min_Tick := Cached.Tick;
                     Result := I;
                  end if;
               end;
            end loop;

            --  The cache was full, so the layout that is replaced is
            --  written back to its page.

            if Save_Cap then
               declare
                  Cache : Cached_Capability renames
                            Table.Cached_Caps (Result);
                  Page  : Capability_Page
                    with Import,
                         Address => Table.Cap_Pages (Page_Index (Cache.Cap));
               begin
                  Page (Cache.Cap mod Capabilities_Per_Page + 1) :=
                    Cache.Layout;
               end;
            end if;
         end;

         declare
            Cached : Cached_Capability renames Table.Cached_Caps (Result);
         begin
            if Cached.Cap /= 0 then
               Table.Is_Cached (Cached.Cap) := False;
            end if;

            declare
               Page : Capability_Page
                 with Import,
                      Address => Table.Cap_Pages (Page_Index (Cap));
            begin
               Table.Cached_Caps (Result) :=
                 Cached_Capability'
                   (Cap    => Cap,
                    Tick   => 0,
                    Layout => Page (Cap mod Capabilities_Per_Page + 1));
            end;
         end;
      end if;

      if Cap in 1 .. Max_Capability_Index then
         Table.Is_Cached (Cap) := True;
      end if;

      Table.Next_Cap_Tick := Table.Next_Cap_Tick + 1;
      Table.Cached_Caps (Result).Tick := Table.Next_Cap_Tick;
      return Result;
   end Load;

   ------------------
   -- Page_Address --
   ------------------

   function Page_Address
     (Table : Cap_Table;
      Page  : Capability_Page_Index)
      return System.Address
   is (Table.Cap_Pages (Page));

   ---------
   -- Set --
   ---------

   procedure Set
     (Table  : in out Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   is
      Index : constant Cached_Capability_Index := Load (Table, Cap);
   begin
      Table.Cached_Caps (Index).Layout := Layout;
   end Set;

   ------------------
   -- Set_Last_Cap --
   ------------------

   procedure Set_Last_Cap
     (Table : in out Cap_Table;
      Last  : Rose.Capabilities.Capability)
   is
   begin
      Table.Last_Cap := Last;
   end Set_Last_Cap;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page
     (Table   : in out Cap_Table;
      Page    : Capability_Page_Index;
      Address : System.Address)
   is
   begin
      Table.Cap_Pages (Page) := Address;
   end Set_Page;

   ------------
   -- Update --
   ------------

   procedure Update
     (Table  : in out Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Update : not null access
        procedure
          (Layout : in out Rose.Capabilities.Layout.Capability_Layout))
   is
      Index : constant Cached_Capability_Index := Load (Table, Cap);
   begin
      Update (Table.Cached_Caps (Index).Layout);
   end Update;

end Rose.Kernel.Cap_Tables;
