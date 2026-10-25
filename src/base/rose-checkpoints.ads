with Rose.Addresses;
with Rose.Limits;
with Rose.Words;

package Rose.Checkpoints is

   --  The memory server's part of a checkpoint log entry: one or more
   --  header pages that list the pages it saved, followed by those
   --  pages in the order they are listed. The last header page has
   --  Is_Last_Header_Page set. The memory server writes this, and the
   --  restore server reads it.

   Header_Page_Magic : constant := 16#7E6A_F178_79CB_E048#;

   type Header_Page_Flags is new Rose.Words.Word_32;

   Is_Last_Header_Page : constant Header_Page_Flags := 16#0001#;

   type Page_Reference is
      record
         Process : Rose.Words.Word_16 := 0;
         Page    : Rose.Addresses.Virtual_Page_Address := 0;
      end record
     with Pack, Size => 40;

   Max_Page_References : constant := (Rose.Limits.Page_Size - 16) / 5;

   type Page_Reference_Array is
     array (Rose.Words.Word_32 range 1 .. Max_Page_References)
     of Page_Reference;

   type Header_Page is
      record
         Magic : Rose.Words.Word_64 := Header_Page_Magic;
         Flags : Header_Page_Flags := 0;
         Count : Rose.Words.Word_32 := 0;
         Pages : Page_Reference_Array;
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

end Rose.Checkpoints;
