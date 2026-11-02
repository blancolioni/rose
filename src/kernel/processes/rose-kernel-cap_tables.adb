package body Rose.Kernel.Cap_Tables is

   use type System.Address;
   use type Rose.Capabilities.Capability;

   function Has_Slot
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Boolean
   is (Cap in 1 .. Max_Capability_Index - 1
       and then Has_Page (Table, Page_Index (Cap)));
   --  There is a place for the layout of Cap, which is when its page has
   --  been set.  There is none for Max_Capability_Index (see Page_Index).

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

   ------------
   -- Layout --
   ------------

   function Layout
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Layout
   is
   begin
      if not Has_Slot (Table, Cap) then
         return (others => <>);
      end if;

      declare
         Page : Capability_Page
           with Import, Address => Table.Cap_Pages (Page_Index (Cap));
      begin
         return Page (Cap mod Capabilities_Per_Page + 1);
      end;
   end Layout;

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
   begin
      if not Has_Slot (Table, Cap) then
         return;
      end if;

      declare
         Page : Capability_Page
           with Import, Address => Table.Cap_Pages (Page_Index (Cap));
      begin
         Page (Cap mod Capabilities_Per_Page + 1) := Layout;
      end;
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
   begin
      if not Has_Slot (Table, Cap) then
         return;
      end if;

      declare
         Page : Capability_Page
           with Import, Address => Table.Cap_Pages (Page_Index (Cap));
      begin
         Update (Page (Cap mod Capabilities_Per_Page + 1));
      end;
   end Update;

end Rose.Kernel.Cap_Tables;
