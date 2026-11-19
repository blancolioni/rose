with Rose.Addresses;

package Mem.Page_Table is

   type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;

   function Is_Mapped (Position : Cursor) return Boolean;
   function Is_Readable (Position : Cursor) return Boolean;
   function Is_Writable (Position : Cursor) return Boolean;
   function Is_Executable (Position : Cursor) return Boolean;
   function Is_Dirty (Position : Cursor) return Boolean;

   function Process
     (Position : Cursor)
      return Process_Id;

   function Virtual_Address
     (Position : Cursor)
      return Rose.Addresses.Virtual_Page_Address;

   function Physical_Address
     (Position : Cursor)
      return Rose.Addresses.Physical_Page_Address;

   procedure Set_Physical_Address
     (Position : Cursor;
      Phys     : Rose.Addresses.Physical_Page_Address);

   procedure Set_Writable (Position : Cursor);
   --  Has_Element (Position) must be True.
   --  Is_Writable (Position) must be False.
   --  Makes the page writable, and dirty. A persistent page that wasn't
   --  dirty is added to the dirty page list.

   procedure Set_Read_Only (Position : Cursor);
   --  Has_Element (Position) must be True.
   --  Makes the page read only. It stays dirty, and in the dirty page
   --  list, until Clear_Dirty_Pages: a checkpoint that fails leaves it
   --  there, to be saved by the next one.

   procedure Insert
     (Process    : Process_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Physical   : Rose.Addresses.Physical_Page_Address;
      Mapped     : Boolean;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean;
      Persistent : Boolean);

   procedure Delete_All
     (Process : Process_Id);

   procedure Delete (Position : Cursor);
   --  Forget the page at Position, without freeing its physical page or
   --  unmapping it. The next checkpoint doesn't save it.

   procedure Sweep_Clean_Pages
     (Try_Page : not null access
        procedure (Position : Cursor;
                   Taken    : out Boolean);
      Position : out Cursor);
   --  Move a clock hand round the table, once at most, offering Try_Page
   --  each page that is mapped, persistent, clean and read only, from the
   --  page after the one the hand last stopped at: the pages that have
   --  been longest since the hand passed them come first (#973). Stop at
   --  the first page that Try_Page takes, and return it, or zero if it
   --  took none. Try_Page must not change the table.

   procedure Iterate_Process_Pages
     (Process : Process_Id;
      Base    : Rose.Addresses.Virtual_Page_Address;
      Bound   : Rose.Addresses.Virtual_Page_Address;
      Action  : not null access procedure (Position : Cursor));
   --  Call Action for each page of Process from Base to Bound - 1. Action
   --  may Delete the page it is given.

   function Find
     (Process    : Process_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address)
      return Cursor;

   procedure Iterate_Dirty_Pages
     (Process : not null access
        procedure (Position : Cursor));

   procedure Clear_Dirty_Pages;
   --  Every page in the dirty page list is clean, and the list is empty:
   --  a checkpoint has saved them.

   function Dirty_Page_Count return Natural;

private

   type Cursor is new Natural;

   function Has_Element (Position : Cursor) return Boolean
   is (Position /= 0);

end Mem.Page_Table;
