package body Rose.Kernel.Cap_Tables
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The invariants, assertions and lemmas are for the proof.

   procedure Lemma_Frame
     (Table  : Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Memory : Rose.Kernel.Cap_Slots.Memory_Image;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   with Ghost,
        Pre  => Consistent (Table) and then Has_Slot (Table, Cap),
        Post =>
          Layout_In
            (Table, Cap,
             Rose.Kernel.Cap_Slots.Updated
               (Memory,
                Page_Address (Table, Page_Index (Cap)),
                Slot_Of (Cap),
                Layout))
          = Layout
          and then
            (for all C in Capability_Number =>
               (if C /= Cap
                then Layout_In
                       (Table, C,
                        Rose.Kernel.Cap_Slots.Updated
                          (Memory,
                           Page_Address (Table, Page_Index (Cap)),
                           Slot_Of (Cap),
                           Layout))
                     = Layout_In (Table, C, Memory)));
   --  Writing the slot of Cap changes the layout of Cap, and no other.

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability)
   is
   begin
      Table.Is_Active (Cap) := True;
   end Activate;

   ------------
   -- Create --
   ------------

   procedure Create
     (Table : in out Cap_Table;
      Cap   : out Rose.Capabilities.Capability)
   is
   begin
      for Number in Capability_Number loop
         pragma Loop_Invariant (Table = Table'Loop_Entry);
         pragma Loop_Invariant
           (for all C in 1 .. Number - 1 => Table.Is_Active (C));

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
      if Is_Active (Table, Cap) then
         Set (Table, Cap, Rose.Capabilities.Layout.Empty_Capability);
         Table.Is_Active (Cap) := False;
      end if;
   end Delete;

   ------------
   -- Layout --
   ------------

   function Layout
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Layout
   is
   begin
      if Has_Slot (Table, Cap) then
         return Rose.Kernel.Cap_Slots.Read
           (Page_Address (Table, Page_Index (Cap)), Slot_Of (Cap));
      end if;

      return Rose.Capabilities.Layout.Empty_Capability;
   end Layout;

   -----------------------
   -- Lemma_Count_Wraps --
   -----------------------

   procedure Lemma_Count_Wraps
     (Count    : Rose.Objects.Rescinded_Count;
      Rescinds : Natural)
   is
   begin
      null;
   end Lemma_Count_Wraps;

   -----------------
   -- Lemma_Frame --
   -----------------

   procedure Lemma_Frame
     (Table  : Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Memory : Rose.Kernel.Cap_Slots.Memory_Image;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   is
      Page : constant System.Address :=
               Page_Address (Table, Page_Index (Cap));
      Slot : constant Rose.Kernel.Cap_Slots.Slot_Index := Slot_Of (Cap);
   begin
      Rose.Kernel.Cap_Slots.Lemma_Updated_Slot (Memory, Page, Slot, Layout);

      for C in Capability_Number loop
         pragma Loop_Invariant
           (for all D in 1 .. C - 1 =>
              (if D /= Cap
               then Layout_In
                      (Table, D,
                       Rose.Kernel.Cap_Slots.Updated
                         (Memory, Page, Slot, Layout))
                    = Layout_In (Table, D, Memory)));

         if C /= Cap and then Has_Slot (Table, C) then

            --  Another page is another address, and in the same page it is
            --  another slot.

            pragma Assert
              (Page_Address (Table, Page_Index (C)) /= Page
               or else Slot_Of (C) /= Slot);

            Rose.Kernel.Cap_Slots.Lemma_Updated_Other
              (Memory, Page, Slot, Layout,
               Page_Address (Table, Page_Index (C)), Slot_Of (C));
         end if;
      end loop;
   end Lemma_Frame;

   -------------------------------
   -- Lemma_Rescind_Invalidates --
   -------------------------------

   procedure Lemma_Rescind_Invalidates
     (Layout : Rose.Capabilities.Layout.Capability_Layout)
   is
   begin
      null;
   end Lemma_Rescind_Invalidates;

   -------------
   -- Rescind --
   -------------

   procedure Rescind
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
   is
   begin
      if Is_Active (Table, Cap) and then Has_Slot (Table, Cap) then
         declare
            Page   : constant System.Address :=
                       Page_Address (Table, Page_Index (Cap));
            Slot   : constant Rose.Kernel.Cap_Slots.Slot_Index :=
                       Slot_Of (Cap);
            Layout : Rose.Capabilities.Layout.Capability_Layout :=
                       Rose.Kernel.Cap_Slots.Read (Page, Slot);
            Before : constant Rose.Kernel.Cap_Slots.Memory_Image :=
                       Rose.Kernel.Cap_Slots.Image
            with Ghost;
         begin
            Layout.Header.Rescinded_Count :=
              Layout.Header.Rescinded_Count + 1;
            Rose.Kernel.Cap_Slots.Write (Page, Slot, Layout);
            Lemma_Frame (Table, Cap, Before, Layout);
         end;
      end if;
   end Rescind;

   ---------
   -- Set --
   ---------

   procedure Set
     (Table  : Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   is
   begin
      if Is_Active (Table, Cap) and then Has_Slot (Table, Cap) then
         declare
            Before : constant Rose.Kernel.Cap_Slots.Memory_Image :=
                       Rose.Kernel.Cap_Slots.Image
            with Ghost;
         begin
            Rose.Kernel.Cap_Slots.Write
              (Page_Address (Table, Page_Index (Cap)), Slot_Of (Cap),
               Layout);
            Lemma_Frame (Table, Cap, Before, Layout);
         end;
      end if;
   end Set;

   --------------------
   -- Set_Identifier --
   --------------------

   procedure Set_Identifier
     (Table      : Cap_Table;
      Cap        : Rose.Capabilities.Capability;
      Identifier : Rose.Objects.Capability_Identifier)
   is
   begin
      if Is_Active (Table, Cap) and then Has_Slot (Table, Cap) then
         declare
            Page   : constant System.Address :=
                       Page_Address (Table, Page_Index (Cap));
            Slot   : constant Rose.Kernel.Cap_Slots.Slot_Index :=
                       Slot_Of (Cap);
            Layout : Rose.Capabilities.Layout.Capability_Layout :=
                       Rose.Kernel.Cap_Slots.Read (Page, Slot);
            Before : constant Rose.Kernel.Cap_Slots.Memory_Image :=
                       Rose.Kernel.Cap_Slots.Image
            with Ghost;
         begin
            Layout.Header.Identifier := Identifier;
            Rose.Kernel.Cap_Slots.Write (Page, Slot, Layout);
            Lemma_Frame (Table, Cap, Before, Layout);
         end;
      end if;
   end Set_Identifier;

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

end Rose.Kernel.Cap_Tables;
