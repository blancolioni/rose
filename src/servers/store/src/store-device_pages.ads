with Rose.Words;

package Store.Device_Pages is

   --  The pages of the backing stores, as the block map and the free map
   --  see them (ADR 0038): one numbering across every backing store.
   --  Page 0 is never a backing store's, and stands for no page.

   type Device_Page is new Rose.Words.Word_32;

   No_Page : constant Device_Page := 0;

   Words_Per_Page : constant := 512;

   type Word_Index is range 0 .. Words_Per_Page - 1;

   type Page_Words is array (Word_Index) of Rose.Words.Word_64
     with Component_Size => 64, Size => Words_Per_Page * 64;
   --  A page, as the words of the tree and the free map.

   function Shift_Left
     (Value  : Rose.Words.Word_64;
      Amount : Natural)
      return Rose.Words.Word_64
     with Import, Convention => Intrinsic;

   function Shift_Right
     (Value  : Rose.Words.Word_64;
      Amount : Natural)
      return Rose.Words.Word_64
     with Import, Convention => Intrinsic;
   --  The kernel run-time system has no 64-bit division, so keys and
   --  bits are found with shifts.

end Store.Device_Pages;
