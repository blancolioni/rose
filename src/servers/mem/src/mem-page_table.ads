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
   --  Adds the referenced page to the dirty page list.
   --  Makes the page writable

   procedure Set_Read_Only (Position : Cursor);
   --  Has_Element (Position) must be True.
   --  Is_Writable (Position) must be True.
   --  Is_Dirty (Position) must be True
   --  Makes the page read only and clears dirty flag

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

   function Find
     (Process    : Process_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address)
      return Cursor;

   procedure Iterate_Dirty_Pages
     (Process : not null access
        procedure (Position : Cursor));

   procedure Clear_Dirty_Pages;

   function Dirty_Page_Count return Natural;

private

   type Cursor is new Natural;

   function Has_Element (Position : Cursor) return Boolean
   is (Position /= 0);

end Mem.Page_Table;
