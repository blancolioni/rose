with System;

with Rose.Capabilities;
with Rose.Capabilities.Layout;
with Rose.Objects;

with Rose.Kernel.Cap_Slots;

package Rose.Kernel.Cap_Tables
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   pragma Unevaluated_Use_Of_Old (Allow);

   --  The capabilities of one process (ADR 0023).  A capability is a
   --  number from 1 to Max_Capability_Index, and is active from Create to
   --  Delete.  Its layout is held in a page: the pages of a table are
   --  allocated by whoever has the table, as it needs them, with Set_Page,
   --  and a layout is read and written in its page, with nothing between,
   --  through Rose.Kernel.Cap_Slots.
   --
   --  There is no output, and nothing here knows of a process.  Reporting
   --  that a table is full, and allocating a page, are done by
   --  Rose.Kernel.Processes, which has the functions that take a process
   --  id.
   --
   --  What is proved of it is in the contracts, and in the invariant of the
   --  table: Last_Cap is at most Max_Capability_Index, a capability that is
   --  active is at most Last_Cap, and no two pages are the same page.

   use type Rose.Capabilities.Capability;
   use type Rose.Objects.Rescinded_Count;
   use type System.Address;
   use type Rose.Capabilities.Layout.Capability_Layout;

   Capabilities_Per_Page : constant :=
                             Rose.Kernel.Cap_Slots.Capabilities_Per_Page;

   subtype Capability_Page is Rose.Kernel.Cap_Slots.Capability_Page;

   Max_Capability_Pages : constant := 16;

   Max_Capability_Index : constant :=
                            Capabilities_Per_Page * Max_Capability_Pages - 1;
   --  The numbers 0 to this are the ones that have a slot, and 0 has none
   --  of its own (it is no capability).

   subtype Capability_Number is
     Rose.Capabilities.Capability range 1 .. Max_Capability_Index;

   type Capability_Page_Count is range 0 .. Max_Capability_Pages;
   subtype Capability_Page_Index is Capability_Page_Count range
     1 .. Capability_Page_Count'Last;

   function Page_Index
     (Cap : Rose.Capabilities.Capability)
      return Capability_Page_Index
   is (Capability_Page_Count (Natural (Cap) / Capabilities_Per_Page) + 1)
   with Pre => Cap <= Max_Capability_Index;
   --  The page that holds the layout of Cap.

   function Slot_Of
     (Cap : Rose.Capabilities.Capability)
      return Rose.Kernel.Cap_Slots.Slot_Index
   is (Cap mod Capabilities_Per_Page + 1);
   --  Where in its page.

   type Cap_Table is private;
   --  A table that is all zeros is empty, which is how a process's entry
   --  starts out (see Rose.Kernel.Processes.Release_Entry).

   function Last_Cap
     (Table : Cap_Table)
      return Rose.Capabilities.Capability;
   --  The highest number that has been in use.

   function Is_Active
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Boolean;
   --  Cap is in use.  This is False for 0, and for a number that is
   --  greater than Max_Capability_Index.

   function Has_Page
     (Table : Cap_Table;
      Page  : Capability_Page_Index)
      return Boolean;

   function Page_Address
     (Table : Cap_Table;
      Page  : Capability_Page_Index)
      return System.Address;
   --  The address of the page, or System.Null_Address if there is none.

   function Has_Slot
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Boolean;
   --  There is a place for the layout of Cap, which is when it has a number
   --  and its page has been set.

   function Layout_In
     (Table  : Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Memory : Rose.Kernel.Cap_Slots.Memory_Image)
      return Rose.Capabilities.Layout.Capability_Layout
   with Ghost;
   --  What the layout of Cap is, in a memory.

   function Rescinded
     (Layout : Rose.Capabilities.Layout.Capability_Layout)
      return Rose.Capabilities.Layout.Capability_Layout
   is (Header  =>
         (Cap_Type         => Layout.Header.Cap_Type,
          Single_Use       => Layout.Header.Single_Use,
          Read_Access      => Layout.Header.Read_Access,
          Write_Access     => Layout.Header.Write_Access,
          Execution_Access => Layout.Header.Execution_Access,
          Rescinded_Count  => Layout.Header.Rescinded_Count + 1,
          Endpoint         => Layout.Header.Endpoint,
          Identifier       => Layout.Header.Identifier),
       Payload => Layout.Payload)
   with Ghost;
   --  Layout, with one more rescind.

   function With_Identifier
     (Layout     : Rose.Capabilities.Layout.Capability_Layout;
      Identifier : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Layout.Capability_Layout
   is (Header  =>
         (Cap_Type         => Layout.Header.Cap_Type,
          Single_Use       => Layout.Header.Single_Use,
          Read_Access      => Layout.Header.Read_Access,
          Write_Access     => Layout.Header.Write_Access,
          Execution_Access => Layout.Header.Execution_Access,
          Rescinded_Count  => Layout.Header.Rescinded_Count,
          Endpoint         => Layout.Header.Endpoint,
          Identifier       => Identifier),
       Payload => Layout.Payload)
   with Ghost;
   --  Layout, with another identifier.

   --  Whether a capability is still valid after the capability that it is
   --  checked against has been rescinded.

   function Same_Count
     (Source : Rose.Capabilities.Layout.Capability_Layout;
      Send   : Rose.Capabilities.Layout.Capability_Layout)
      return Boolean
   is (Source.Header.Rescinded_Count = Send.Header.Rescinded_Count);
   --  A call through Source, to an endpoint whose own capability is Send,
   --  is valid.

   function Count_After
     (Count    : Rose.Objects.Rescinded_Count;
      Rescinds : Natural)
      return Rose.Objects.Rescinded_Count
   is (Count + Rose.Objects.Rescinded_Count (Rescinds mod 2 ** 24))
   with Ghost;
   --  The count of a capability that has been rescinded Rescinds times.

   procedure Lemma_Rescind_Invalidates
     (Layout : Rose.Capabilities.Layout.Capability_Layout)
   with Ghost,
        Post => not Same_Count (Layout, Rescinded (Layout));
   --  A capability that is rescinded is not valid.

   procedure Lemma_Count_Wraps
     (Count    : Rose.Objects.Rescinded_Count;
      Rescinds : Natural)
   with Ghost,
        Post => (Count_After (Count, Rescinds) = Count)
                = (Rescinds mod 2 ** 24 = 0);
   --  ... unless the count has gone all the way round, which is after a
   --  multiple of 2 ** 24 rescinds.

   procedure Set_Page
     (Table   : in out Cap_Table;
      Page    : Capability_Page_Index;
      Address : System.Address)
   with Pre  => Address /= System.Null_Address
                and then not Has_Page (Table, Page)
                and then (for all P in Capability_Page_Index =>
                            Page_Address (Table, P) /= Address),
        Post => Page_Address (Table, Page) = Address
                and then (for all P in Capability_Page_Index =>
                            (if P /= Page
                             then Page_Address (Table, P)
                                  = Page_Address (Table'Old, P)))
                and then Last_Cap (Table) = Last_Cap (Table'Old)
                and then (for all C in Capability_Number =>
                            Is_Active (Table, C)
                            = Is_Active (Table'Old, C));

   function Layout
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Layout
   with Post => Layout'Result
                = Layout_In (Table, Cap, Rose.Kernel.Cap_Slots.Image);
   --  The layout of Cap, which is the empty layout if it was never set,
   --  and if there is no page for it, or no number.  It does not need the
   --  page to have been set.

   procedure Set
     (Table  : Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   with Post => Layout_In (Table, Cap, Rose.Kernel.Cap_Slots.Image)
                         = (if Has_Slot (Table, Cap)
                            then Layout
                            else Rose.Capabilities.Layout.Empty_Capability)
                and then (for all C in Capability_Number =>
                            (if C /= Cap
                             then Layout_In
                                    (Table, C, Rose.Kernel.Cap_Slots.Image)
                                  = Layout_In
                                      (Table, C,
                                       Rose.Kernel.Cap_Slots.Image'Old)));
   --  Set the layout of Cap.  It does nothing if there is no page for it,
   --  which the caller has to allocate first.  No other capability changes.

   procedure Rescind
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
   with Post => (if Has_Slot (Table, Cap)
                   then Layout_In (Table, Cap, Rose.Kernel.Cap_Slots.Image)
                        = Rescinded
                            (Layout_In
                               (Table, Cap,
                                Rose.Kernel.Cap_Slots.Image'Old)))
                and then (for all C in Capability_Number =>
                            (if C /= Cap
                             then Layout_In
                                    (Table, C, Rose.Kernel.Cap_Slots.Image)
                                  = Layout_In
                                      (Table, C,
                                       Rose.Kernel.Cap_Slots.Image'Old)));
   --  Add one to the rescinded count of Cap, which makes every capability
   --  that was checked against it as it was invalid.  It does nothing if
   --  there is no page for it.

   procedure Set_Identifier
     (Table      : Cap_Table;
      Cap        : Rose.Capabilities.Capability;
      Identifier : Rose.Objects.Capability_Identifier)
   with Post => (if Has_Slot (Table, Cap)
                   then Layout_In (Table, Cap, Rose.Kernel.Cap_Slots.Image)
                        = With_Identifier
                            (Layout_In
                               (Table, Cap,
                                Rose.Kernel.Cap_Slots.Image'Old),
                             Identifier))
                and then (for all C in Capability_Number =>
                            (if C /= Cap
                             then Layout_In
                                    (Table, C, Rose.Kernel.Cap_Slots.Image)
                                  = Layout_In
                                      (Table, C,
                                       Rose.Kernel.Cap_Slots.Image'Old)));
   --  Give Cap another identifier.  It does nothing if there is no page
   --  for it.

   procedure Create
     (Table : in out Cap_Table;
      Cap   : out Rose.Capabilities.Capability)
   with Post => Last_Cap (Table) >= Last_Cap (Table'Old)
                and then
                  (if Cap /= 0
                   then Cap in Capability_Number
                        and then not Is_Active (Table'Old, Cap)
                        and then Is_Active (Table, Cap)
                        and then (for all C in 1 .. Cap - 1 =>
                                    Is_Active (Table'Old, C))
                        and then (for all C in Capability_Number =>
                                    (if C /= Cap
                                     then Is_Active (Table, C)
                                          = Is_Active (Table'Old, C)))
                   else (for all C in Capability_Number =>
                           Is_Active (Table'Old, C)
                           and then Is_Active (Table, C)));
   --  The lowest number that is not in use, which is now in use, or 0 if
   --  there is none.  Its layout is whatever it was: Delete empties it.

   procedure Delete
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability)
   with Post => not Is_Active (Table, Cap)
                and then Last_Cap (Table) = Last_Cap (Table'Old)
                and then Layout_In (Table, Cap, Rose.Kernel.Cap_Slots.Image)
                         = Rose.Capabilities.Layout.Empty_Capability
                and then (for all C in Capability_Number =>
                            (if C /= Cap
                             then Is_Active (Table, C)
                                  = Is_Active (Table'Old, C)
                                  and then Layout_In
                                             (Table, C,
                                              Rose.Kernel.Cap_Slots.Image)
                                           = Layout_In
                                               (Table'Old, C,
                                                Cap_Slots.Image'Old)));
   --  Empty the layout of Cap, and take it out of use.

   procedure Set_Last_Cap
     (Table : in out Cap_Table;
      Last  : Rose.Capabilities.Capability)
   with Pre  => Last in Last_Cap (Table) .. Max_Capability_Index,
        Post => Last_Cap (Table) = Last
                and then (for all C in Capability_Number =>
                            Is_Active (Table, C)
                            = Is_Active (Table'Old, C));
   --  For a restore, which puts the capabilities back at their numbers.

   procedure Activate
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability)
   with Pre  => Cap in 1 .. Last_Cap (Table),
        Post => Is_Active (Table, Cap)
                and then Last_Cap (Table) = Last_Cap (Table'Old)
                and then (for all C in Capability_Number =>
                            (if C /= Cap
                             then Is_Active (Table, C)
                                  = Is_Active (Table'Old, C)));
   --  Put Cap in use, with the layout that it has.  Set_Last_Cap comes
   --  first.

private

   type Capability_Page_Array is
     array (Capability_Page_Index) of System.Address;

   type Cap_Present_Array is
     array (Capability_Number) of Boolean
     with Pack, Size => Max_Capability_Index;

   type Cap_Table is
      record
         Last_Cap  : Rose.Capabilities.Capability := 0;
         Is_Active : Cap_Present_Array := (others => False);
         Cap_Pages : Capability_Page_Array :=
                       (others => System.Null_Address);
      end record
   with Type_Invariant => Consistent (Cap_Table);

   function Consistent (Table : Cap_Table) return Boolean
   is (Table.Last_Cap <= Max_Capability_Index
       and then (for all C in Capability_Number =>
                   (if Table.Is_Active (C) then C <= Table.Last_Cap))
       and then (for all P in Capability_Page_Index =>
                   (for all Q in Capability_Page_Index =>
                      (if P /= Q
                       and then Table.Cap_Pages (P) /= System.Null_Address
                       then Table.Cap_Pages (P) /= Table.Cap_Pages (Q)))));

   function Last_Cap
     (Table : Cap_Table)
      return Rose.Capabilities.Capability
   is (Table.Last_Cap);

   function Is_Active
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Boolean
   is (Cap in Capability_Number and then Table.Is_Active (Cap));

   function Has_Page
     (Table : Cap_Table;
      Page  : Capability_Page_Index)
      return Boolean
   is (Table.Cap_Pages (Page) /= System.Null_Address);

   function Page_Address
     (Table : Cap_Table;
      Page  : Capability_Page_Index)
      return System.Address
   is (Table.Cap_Pages (Page));

   function Has_Slot
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Boolean
   is (Cap in Capability_Number
       and then Has_Page (Table, Page_Index (Cap)));

   function Layout_In
     (Table  : Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Memory : Rose.Kernel.Cap_Slots.Memory_Image)
      return Rose.Capabilities.Layout.Capability_Layout
   is (if Has_Slot (Table, Cap)
       then Rose.Kernel.Cap_Slots.Value
              (Memory,
               Page_Address (Table, Page_Index (Cap)),
               Slot_Of (Cap))
       else Rose.Capabilities.Layout.Empty_Capability);

end Rose.Kernel.Cap_Tables;
