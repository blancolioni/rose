with Rose.Addresses;
with Rose.Limits;
with Rose.Words;

package Mem.Change_Table is

   --  The table of changes since the last checkpoint (ADR 0028, section
   --  3; #974): for each page paged out to the open entry since then,
   --  the process, the virtual page and the log page of its latest copy.
   --  The next checkpoint adds it to the index, and its commit empties
   --  it.

   Capacity : constant := Rose.Limits.Change_Table_Entries;

   function Count return Natural;

   function Room return Natural;
   --  How many more pages can be added. One slot is always left empty,
   --  so that a search ends.

   function Wants_Checkpoint return Boolean
   is (Count >= Capacity * 3 / 4);
   --  The table is three quarters full, so mem asks for a checkpoint now
   --  (ADR 0028, section 6).

   procedure Add
     (Process  : Process_Id;
      Page     : Rose.Addresses.Virtual_Page_Address;
      Log_Page : Rose.Words.Word_32);
   --  Page of Process is at Log_Page, replacing any earlier copy. Room
   --  must not be 0. Log_Page is not 0.

   function Find
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Rose.Words.Word_32;
   --  The log page of Page of Process, or 0 if the table doesn't have it.

   procedure Forget (Process : Process_Id);
   --  Remove the pages of Process, whose slot has a new process or none.

   procedure Clear;
   --  A checkpoint has committed: the index names every page.

   procedure Iterate
     (Process : not null access
        procedure (Owner    : Process_Id;
                   Page     : Rose.Addresses.Virtual_Page_Address;
                   Log_Page : Rose.Words.Word_32));
   --  Call Process for each page in the table, in no particular order.

end Mem.Change_Table;
