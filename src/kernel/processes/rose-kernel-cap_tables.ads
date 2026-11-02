with System;

with Rose.Addresses;
with Rose.Capabilities;
with Rose.Capabilities.Layout;

package Rose.Kernel.Cap_Tables is

   --  The capabilities of one process (ADR 0023).  A capability is a
   --  number from 1 to Max_Capability_Index, and is active from Create to
   --  Delete.  Its layout is held in a page: the pages of a table are
   --  allocated by whoever has the table, as it needs them, with Set_Page,
   --  and a layout is read and written in its page, with nothing between.
   --
   --  There is no output, and nothing here knows of a process.  Reporting
   --  that a table is full, and allocating a page, are done by
   --  Rose.Kernel.Processes, which has the functions that take a process
   --  id.

   Capabilities_Per_Page : constant :=
                   Rose.Addresses.Physical_Page_Bytes
                 / Rose.Capabilities.Layout.Capability_Layout_Bytes;

   type Capability_Page is
     array (Rose.Capabilities.Capability range 1 .. Capabilities_Per_Page)
     of Rose.Capabilities.Layout.Capability_Layout
       with Size => Rose.Addresses.Physical_Page_Bits;

   Max_Capability_Pages : constant := 16;
   Max_Capability_Index : constant :=
                            Capabilities_Per_Page * Max_Capability_Pages;

   type Capability_Page_Count is range 0 .. Max_Capability_Pages;
   subtype Capability_Page_Index is Capability_Page_Count range
     1 .. Capability_Page_Count'Last;

   function Page_Index
     (Cap : Rose.Capabilities.Capability)
      return Capability_Page_Index
   is (Capability_Page_Count (Natural (Cap) / Capabilities_Per_Page) + 1);
   --  The page that holds the layout of Cap.  That is a page that there is
   --  only for the numbers below Max_Capability_Index: the page of
   --  Max_Capability_Index itself would be the one after the last.

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
   --  greater than Last_Cap.

   function Has_Page
     (Table : Cap_Table;
      Page  : Capability_Page_Index)
      return Boolean;

   function Page_Address
     (Table : Cap_Table;
      Page  : Capability_Page_Index)
      return System.Address;
   --  The address of the page, or System.Null_Address if there is none.

   procedure Set_Page
     (Table   : in out Cap_Table;
      Page    : Capability_Page_Index;
      Address : System.Address);

   function Layout
     (Table : Cap_Table;
      Cap   : Rose.Capabilities.Capability)
      return Rose.Capabilities.Layout.Capability_Layout;
   --  The layout of Cap, which is the empty layout if it was never set,
   --  and if there is no page for it, or no number.  It does not need the
   --  page to have been set.

   procedure Set
     (Table  : in out Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Layout : Rose.Capabilities.Layout.Capability_Layout);
   --  Set the layout of Cap.  It does nothing if there is no page for it,
   --  which the caller has to allocate first.

   procedure Update
     (Table  : in out Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Update : not null access
        procedure
          (Layout : in out Rose.Capabilities.Layout.Capability_Layout));
   --  Change the layout of Cap in place.  It does nothing if there is no
   --  page for it.

   procedure Create
     (Table : in out Cap_Table;
      Cap   : out Rose.Capabilities.Capability);
   --  The lowest number that is not in use, which is now in use, or 0 if
   --  there is none.  Its layout is whatever it was: Delete empties it.

   procedure Delete
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability);
   --  Empty the layout of Cap, and take it out of use.

   procedure Set_Last_Cap
     (Table : in out Cap_Table;
      Last  : Rose.Capabilities.Capability);

   procedure Activate
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability);
   --  Put Cap in use, with the layout that it has, as a restore puts a
   --  capability back at its number.  Set_Last_Cap comes first.

private

   type Capability_Page_Array is
     array (Capability_Page_Index) of System.Address;

   type Cap_Present_Array is
     array (Rose.Capabilities.Capability range 1 .. Max_Capability_Index)
     of Boolean
     with Pack, Size => Max_Capability_Index;

   type Cap_Table is
      record
         Last_Cap  : Rose.Capabilities.Capability := 0;
         Is_Active : Cap_Present_Array := (others => False);
         Cap_Pages : Capability_Page_Array :=
                       (others => System.Null_Address);
      end record;

end Rose.Kernel.Cap_Tables;
