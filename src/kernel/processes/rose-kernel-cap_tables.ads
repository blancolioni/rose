with System;

with Rose.Addresses;
with Rose.Capabilities;
with Rose.Capabilities.Layout;
with Rose.Words;

package Rose.Kernel.Cap_Tables is

   --  The capabilities of one process (ADR 0023).  A capability is a
   --  number from 1 to Max_Capability_Index, and is active from Create to
   --  Delete.  Its layout is held in a page: the pages of a table are
   --  allocated by whoever has the table, as it needs them, with Set_Page,
   --  and the operations that read or write a layout need the page that
   --  it is in.
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
   --  The page that holds the layout of Cap.

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

   procedure Get
     (Table  : in out Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Layout : out Rose.Capabilities.Layout.Capability_Layout);
   --  The layout of Cap, which is the empty layout if it was never set.
   --  The page that holds it must have been set.

   procedure Set
     (Table  : in out Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Layout : Rose.Capabilities.Layout.Capability_Layout);

   procedure Update
     (Table  : in out Cap_Table;
      Cap    : Rose.Capabilities.Capability;
      Update : not null access
        procedure
          (Layout : in out Rose.Capabilities.Layout.Capability_Layout));
   --  Change the layout of Cap in place.

   procedure Create
     (Table : in out Cap_Table;
      Cap   : out Rose.Capabilities.Capability);
   --  The lowest number that is not in use, which is now in use, or 0 if
   --  there is none.  Its layout is whatever it was: Delete empties it.

   procedure Delete
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability);
   --  Empty the layout of Cap, and take it out of use.  The page that
   --  holds it must have been set.

   procedure Set_Last_Cap
     (Table : in out Cap_Table;
      Last  : Rose.Capabilities.Capability);

   procedure Activate
     (Table : in out Cap_Table;
      Cap   : Rose.Capabilities.Capability);
   --  Put Cap in use, with the layout that it has, as a restore puts a
   --  capability back at its number.  Set_Last_Cap comes first.

private

   Capability_Cache_Size : constant := 16;
   --  The layouts of the last capabilities that were used are kept in
   --  the table, so that they need not be found in their pages.

   type Cached_Capability_Count is range 0 .. Capability_Cache_Size;
   subtype Cached_Capability_Index is Cached_Capability_Count range
     1 .. Cached_Capability_Count'Last;

   type Cached_Capability is
      record
         Cap    : Rose.Capabilities.Capability := 0;
         Tick   : Rose.Words.Word_32 := 0;
         Layout : Rose.Capabilities.Layout.Capability_Layout;
      end record;

   type Capability_Cache_Array is
     array (Cached_Capability_Index) of Cached_Capability;

   type Capability_Page_Array is
     array (Capability_Page_Index) of System.Address;

   type Cap_Present_Array is
     array (Rose.Capabilities.Capability range 1 .. Max_Capability_Index)
     of Boolean
     with Pack, Size => Max_Capability_Index;

   type Cap_Table is
      record
         Last_Cap      : Rose.Capabilities.Capability := 0;
         Cached_Caps   : Capability_Cache_Array := (others => <>);
         Is_Cached     : Cap_Present_Array := (others => False);
         Is_Active     : Cap_Present_Array := (others => False);
         Next_Cap_Tick : Rose.Words.Word_32 := 0;
         Cap_Pages     : Capability_Page_Array :=
                           (others => System.Null_Address);
      end record;

end Rose.Kernel.Cap_Tables;
